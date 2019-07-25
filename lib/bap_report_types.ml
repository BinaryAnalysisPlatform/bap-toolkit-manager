open Core_kernel

type status =
  | Confirmed
  | False_pos
  | False_neg
  | Undecided
[@@deriving sexp]

type stat = {
  false_pos : int;
  false_neg : int;
  confirmed : int;
  undecided : int;
}

type check =
  | Unused_return_value
  | Null_ptr_deref
  | Forbidden_function
  | Complex_function
  | Non_structural_cfg
  | Recursive_function
  | Hardcoded_socket_address
  | Memcheck_double_release
  | Memcheck_out_of_bound
  | Memcheck_use_after_release
  | Untrusted_argument
[@@deriving bin_io, compare, sexp]


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

type result = string list [@@deriving bin_io,sexp]

module Result = struct

  let rec compare_strings xs ys =
    match xs, ys with
    | [],[] -> 0
    | x :: xs, y :: ys ->
      let r = String.compare x y in
      if r = 0 then compare xs ys
      else r
    | [], _ -> 1
    | _, [] -> -1


  module Cmp = struct
    type t = result [@@deriving bin_io,sexp]
    let compare = compare_strings
    include Comparator.Make(struct
        type nonrec t = t [@@deriving bin_io,sexp]
        let compare = compare
      end)
  end

  include Cmp

  module Map = Map.Make(Cmp)
end

type artifact = {
  name : string;
  size : int option;
  data : status Result.Map.t Check.Map.t;
  time : float Check.Map.t;
}

module Size = struct
  type t = int

  let to_string_hum bytes =
    let kbytes = bytes / 1024 in
    let mbytes = kbytes / 1024 in
    match mbytes, kbytes, bytes with
    | 0,0,b -> sprintf "%d bytes" b
    | 0,k,_ -> sprintf "%dK" k
    | m,_,_ -> sprintf "%dM" m

end

module Time = struct
  type t = float

  let to_string_hum secs =
    let secs = int_of_float secs in
    let hours = secs / 3600 in
    let minut = (secs - hours * 3600) / 60 in
    let secnd  = (secs - hours * 3600 - minut * 60) in
    sprintf "%02d:%02d:%02d" hours minut secnd

end

module Artifact = struct

  type t = artifact

  let create ?size name = {
    name; size; data = Check.Map.empty; time = Check.Map.empty;
  }

  let name t = t.name
  let size t = t.size

  let size_hum t =
    match t.size with
    | None -> None
    | Some x -> Some (Size.to_string_hum x)

  let checks {data} =
    Map.to_alist data |>
    List.map ~f:(fun (check, data) -> check, Map.to_alist data)

  let update t check result status =
    {t with
      data =
        Map.update t.data check ~f:(function
            | None -> Result.Map.singleton result status
            | Some res ->
               Map.update res result ~f:(function
                   | None -> status
                   | Some status' when status' = Undecided -> status
                   | Some status' -> status'))}

  let find_check t check =
    match Map.find t.data check with
    | None -> []
    | Some x -> Map.to_alist x

  let find_status t check res =
    match Map.find t.data check with
    | None -> None
    | Some x -> Map.find x res

  let with_size t size = {t with size=Some size}
  let with_time t check time = {t with time = Map.set t.time check time}

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
          ~f:(fun ~key ~data:status (fp,fn,cn,un) ->
              match status with
              | False_pos -> fp + 1, fn, cn, un
              | False_neg -> fp, fn + 1, cn, un
              | Confirmed -> fp, fn, cn + 1, un
              | Undecided -> fp, fn, cn, un + 1) in
      {false_pos; false_neg; confirmed; undecided;}

  let notify_merge_error arti check reason =
    let where = match check with
      | None -> arti
      | Some check ->
         sprintf "%s/%s"
           arti (Sexp.to_string (sexp_of_check check)) in
    eprintf
      "got error while merging results for %s: %s\n"
      where reason

  let merge_results name check x y =
    Map.merge x y ~f:(fun ~key:result -> function
        | `Left s | `Right s -> Some s
        | `Both (ls, rs) ->
           match ls,rs with
           | Undecided,rs -> Some rs
           | ls, Undecided -> Some ls
           | ls, rs ->
              notify_merge_error name (Some check) @@
                sprintf "%s is marked both as %s and %s, Undecided then\n"
                (List.fold result ~init:"" ~f:(sprintf "%s%s "))
                (Sexp.to_string (sexp_of_status ls))
                (Sexp.to_string (sexp_of_status rs));
                Some Undecided)

  let merge_data name data data' =
    Map.merge data data' ~f:(fun ~key:check -> function
        | `Left r | `Right r -> Some r
        | `Both (ls,rs) -> Some (merge_results name check ls rs))

  let merge_time name t t' =
    Map.merge t t'
      ~f:(fun ~key:check -> function
        | `Left x | `Right x -> Some x
        | `Both (x,y) when Float.(x = y) -> Some x
        | `Both (x,y) ->
           notify_merge_error name (Some check) "time is different, dropping both ... ";
           None)

  let merge_size name s s' =
    match s, s' with
    | None, Some s
    | Some s, None -> Some s
    | Some s, Some s' when Int.(s = s') -> Some s
    | None, None -> None
    | _ ->
       notify_merge_error name None "size is different, dropping both ... ";
       None

  let merge t t' =
    if String.(t.name <> t'.name) then None
    else
      let size = merge_size t.name t.size t'.size in
      let time = merge_time t.name t.time t'.time in
      let data = merge_data t.name t.data t'.data in
      Some ({name=t.name; size; time; data})

end
