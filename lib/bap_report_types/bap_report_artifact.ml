open Core_kernel
open Bap_report_common

module Incident = Bap_report_incident
module Kind = Incident.Kind
module Id = Incident.Id


type incident = Incident.t [@@deriving bin_io, compare, sexp]
type incident_kind = Bap_report_incident.kind  [@@deriving bin_io, compare, sexp]
type incident_id = Bap_report_incident.id  [@@deriving bin_io, compare, sexp]

type t = {
  name : string;
  size : int option;
  data : (incident * status) Id.Map.t Kind.Map.t;
  time : float Kind.Map.t;
} [@@deriving bin_io, compare, sexp]

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
  name; size; data = Kind.Map.empty; time = Kind.Map.empty;
}

let name t = t.name
let size t = t.size

let size_hum t =
  match t.size with
  | None -> None
  | Some x -> Some (Size.to_string_hum x)

let checks t = Map.to_alist t.data |> List.map ~f:fst

let update t incident status =
  let kind = Incident.kind incident in
  let id = Incident.id incident in
  {t with
   data =
     Map.update t.data kind ~f:(function
         | None -> Id.Map.singleton id (incident,status)
         | Some res ->
           Map.update res id ~f:(function
               | None -> incident,status
               | Some (inc,status') when status' = Undecided -> inc,status
               | Some x -> x))}

let no_incidents t kind =
  {t with data = Map.set t.data kind (Map.empty (module Id))}


let find_result t kind =
  match Map.find t.data kind with
  | None -> []
  | Some x -> Map.to_alist x

let with_size t size = {t with size=Some size}
let with_time t kind time = {t with time = Map.set t.time ~key:kind ~data:time}

let time t kind = Map.find t.time kind

let time_hum t kind =
  match time t kind with
  | None -> None
  | Some time -> Some (Time.to_string_hum time)

let summary t kind =
  match Map.find t.data kind with
  | None -> {false_pos=0; false_neg=0; confirmed=0; undecided=0;}
  | Some x ->
    let false_pos,false_neg,confirmed,undecided =
      Map.fold x ~init:(0,0,0,0)
        ~f:(fun ~key:_ ~data:(_,status) (fp,fn,cn,un) ->
            match status with
            | False_pos -> fp + 1, fn, cn, un
            | False_neg -> fp, fn + 1, cn, un
            | Confirmed -> fp, fn, cn + 1, un
            | Undecided -> fp, fn, cn, un + 1) in
    {false_pos; false_neg; confirmed; undecided;}

let incidents ?kind t =
  match kind with
  | None ->
    let incs = Map.data t.data |> List.map ~f:Map.data in
    List.concat incs
  | Some kind ->
    match Map.find t.data kind with
    | None -> []
    | Some incs -> Map.data incs


let find t id =
  let kind = Id.kind id in
  match Map.find t.data kind with
  | None -> None
  | Some incs -> Map.find incs id

let merge_name a a' =
  if String.(a.name <> a'.name) then None
  else Some a.name

let merge_size a a' =
  match a.size, a'.size with
  | Some s, Some s' when Int.(s = s') -> Some s
  | Some s, None | None, Some s -> Some s
  | _ -> None

let merge_time a a' =
  Map.merge a.time a'.time ~f:(fun ~key:_ -> function
      | `Left x | `Right x -> Some x
      | `Both (x,y) when Int.(int_of_float x = int_of_float y) -> Some x
      | _ -> None)

let merge_status s s' = match s,s' with
  | Undecided, x | x, Undecided -> Some x
  | _ -> None

let merge_data a a' =
  Map.merge a.data a'.data ~f:(fun ~key:_ -> function
      | `Left x | `Right x -> Some x
      | `Both (x,y) ->
        Option.some @@
        Map.merge x y ~f:(fun ~key:_ -> function
            | `Left x | `Right x -> Some x
            | `Both ((i,s),(_,s')) ->
              match merge_status s s' with
              | Some s -> Some (i,s)
              | None -> None))

let merge a a' =
  match merge_name a a' with
  | None -> None
  | Some name ->
    let size = merge_size a a' in
    let time = merge_time a a' in
    let data = merge_data a a' in
    Some {name; size; time; data}
