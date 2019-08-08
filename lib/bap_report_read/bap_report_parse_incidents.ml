open Core_kernel

open Bap_report_types
open Bap_report_read_types

let some = Option.some

let read_sexp ch =
  try Sexp.input_sexp ch |> some
  with _ -> None

open Sexp

let addr_of_string str =
  match String.split str ~on:':' with
  | x :: _ ->
    let prefix = "0x" in
    let x =
      if String.is_prefix x ~prefix:"0x" then
        String.chop_prefix_exn x ~prefix
      else x in
    prefix ^ String.lowercase x
  | _ -> str

let point_of_sexp x = match x with
  | List _ -> None
  | Atom x ->
    match String.split ~on:':' x  with
    | [_; addr] -> Some (addr_of_string addr)
    | [addr] -> Some (addr_of_string addr)
    | _ -> None

let trace_of_sexps xs =
  List.filter_map ~f:point_of_sexp xs

let locs_of_sexps xs =
  List.filter_map xs ~f:(function
      | Atom s -> Some s
      | _ -> None)

let of_sexp s = match s with
  | List (Atom "incident-location" :: List [Atom loc_id; List points] :: _)  ->
    Incident_location (loc_id, trace_of_sexps points) |> some
  | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
    Incident (name, locs_of_sexps locs) |> some
  | List (Atom "machine-switch" :: List [Atom from; Atom to_ ] :: _ ) ->
    Switch (from,to_) |> some
  | List (Atom "machine-fork" :: List [Atom from; Atom to_ ] :: _ ) ->
    Fork (from,to_) |> some
  | List (Atom "call" :: List (Atom name :: _) :: _ ) ->
    Call name |> some
  | List (Atom "call-return" :: List (Atom name :: _) :: _ ) ->
    Call_return name |> some
  | List (Atom "symbol" :: List [Atom name; Atom addr] :: _) ->
    Symbol (name, addr_of_string addr) |> some
  | List (Atom "pc-changed" :: Atom addr :: _) ->
    Pc_changed (addr_of_string addr) |> some
  | _ -> None

let of_sexp s =
  try
    of_sexp s
  with _ -> None

let rec read ch =
  match read_sexp ch with
  | None -> None
  | Some s ->
    match of_sexp s with
    | None -> read ch
    | Some _ as r -> r


let of_sexp_opt f s =
  try f s |> Option.some
  with _ -> None

let status_of_sexp  s = of_sexp_opt status_of_sexp s
let check_of_sexp   s = of_sexp_opt check_of_sexp s

let read_confirmations ch =
  let rec read acc =
    match read_sexp ch with
    | None -> acc
    | Some (List [status; check; List locs]) ->
       let x =
         Option.(status_of_sexp status >>= fun status ->
                 check_of_sexp check >>= fun check ->
                 let locs = List.map ~f:string_of_sexp locs in
                 let locs = List.map ~f:addr_of_string locs in
                 some @@ (status,check,locs)) in
       read (x :: acc)
    | _ -> read acc in
  read [] |>
    List.filter_map ~f:(function
        | None -> None
        | Some (status,check,locs) ->
           Some (Incident.create check locs, status))
