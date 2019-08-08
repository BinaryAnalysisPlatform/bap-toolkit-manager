open Core_kernel
open Bap_report_common

module Incident = Bap_report_incident
type incident = Incident.t


module Check = struct
  module Cmp = struct
    type nonrec t = check [@@deriving bin_io, compare, sexp]
    include Comparator.Make(struct
        type nonrec t = t [@@deriving bin_io, compare, sexp]
      end)
  end
  include Cmp

  module Map = Map.Make(Cmp)
  module Set = Set.Make(Cmp)

end

type t = {
  name : string;
  size : int option;
  data : status Incident.Map.t Check.Map.t;
  time : float Check.Map.t;
}

module Size = struct

  let to_string_hum bytes =
    let kbytes = bytes / 1024 in
    let mbytes = kbytes / 1024 in
    match mbytes, kbytes, bytes with
    | 0,0,b -> sprintf "%d bytes" b
    | 0,k,_ -> sprintf "%dK" k
    | m,_,_ -> sprintf "%dM" m

end

module Time = struct

  let to_string_hum secs =
    let secs = int_of_float secs in
    let hours = secs / 3600 in
    let minut = (secs - hours * 3600) / 60 in
    let secnd  = (secs - hours * 3600 - minut * 60) in
    sprintf "%02d:%02d:%02d" hours minut secnd

end


let create ?size name  = {
  name; size; data = Check.Map.empty; time = Check.Map.empty;
}

let name t = t.name
let size t = t.size

let size_hum t =
  match t.size with
  | None -> None
  | Some x -> Some (Size.to_string_hum x)

let checks t = Map.to_alist t.data |> List.map ~f:fst

let update t incident status =
  let check = Incident.check incident in
  {t with
   data =
     Map.update t.data check ~f:(function
         | None -> Incident.Map.singleton incident status
         | Some res ->
            Map.update res incident ~f:(function
               | None -> status
               | Some status' when status' = Undecided -> status
               | Some status' -> status'))}

let find_result t check =
  match Map.find t.data check with
  | None -> []
  | Some x -> Map.to_alist x

let with_size t size = {t with size=Some size}
let with_time t check time = {t with time = Map.set t.time ~key:check ~data:time}

let time t check = Map.find t.time check

let time_hum t check =
  match time t check with
  | None -> None
  | Some time -> Some (Time.to_string_hum time)

let summary t check =
  match Map.find t.data check with
  | None -> {false_pos=0; false_neg=0; confirmed=0; undecided=0;}
  | Some x ->
    let false_pos,false_neg,confirmed,undecided =
      Map.fold x ~init:(0,0,0,0)
        ~f:(fun ~key:_ ~data:status (fp,fn,cn,un) ->
            match status with
            | False_pos -> fp + 1, fn, cn, un
            | False_neg -> fp, fn + 1, cn, un
            | Confirmed -> fp, fn, cn + 1, un
            | Undecided -> fp, fn, cn, un + 1) in
    {false_pos; false_neg; confirmed; undecided;}

let incidents ?check t =
  match check with
  | None ->
     let incs = Map.data t.data |> List.map ~f:Map.to_alist in
     List.concat incs
  | Some check ->
     match Map.find t.data check with
     | None -> []
     | Some incs -> Map.to_alist incs
