open Core_kernel

open Bap_report_types
open Bap_report_read_types

let some = Option.some

let read_sexp ch =
  try Sexp.input_sexp ch |> some
  with _ -> None


let point_of_sexp x = match x with
  | Sexp.List _ -> None
  | Sexp.Atom x ->
    match String.split ~on:':' x  with
    | [_; addr] -> Some (Addr.of_string addr)
    | [addr] -> Some (Addr.of_string addr)
    | _ -> None

let trace_of_sexps xs =
  List.filter_map ~f:point_of_sexp xs

let locs_of_sexps xs =
  List.filter_map xs ~f:(function
      | Sexp.Atom s -> Some (Location_id.of_string s)
      | _ -> None)


let of_sexp s =
  let open Sexp in
  match s with
  | List (Atom "incident-location" :: List [Atom id; List points] :: _)  ->
     Incident_location (Location_id.of_string id, trace_of_sexps points) |> some
  | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
     Incident (Incident.Kind.of_string name, locs_of_sexps locs) |> some
  | List (Atom "machine-switch" :: List [Atom from; Atom to_ ] :: _ ) ->
     Switch (Machine_id.of_string from, Machine_id.of_string to_) |> some
  | List (Atom "machine-fork" :: List [Atom from; Atom to_ ] :: _ ) ->
     Fork (Machine_id.of_string from, Machine_id.of_string to_) |> some
  | List (Atom "call" :: List (Atom name :: _) :: _ ) ->
     Call name |> some
  | List (Atom "call-return" :: List (Atom name :: _) :: _ ) ->
     Call_return name |> some
  | List (Atom "symbol" :: List [Atom name; Atom addr] :: _) ->
     Symbol (name, Addr.of_string addr) |> some
  | List (Atom "pc-changed" :: Atom addr :: _) ->
     Pc_changed (Addr.of_string addr) |> some
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

let try_sexp f s = Option.try_with (fun () -> f s)

let try_f f x =
 Option.try_with (fun () -> f x)

let (>>=) = Option.(>>=)

let read_confirmations ch =
  let split s =
    String.split s ~on:' ' |>
      List.map ~f:String.strip |>
      List.filter ~f:(fun s -> s <> "") in
  let rec parse m = function
    | [] -> m
    | line :: lines ->
       match split line with
       | arti :: rel :: kind :: addr :: addrs ->
          let addr  = Addr.of_string addr in
          let addrs = List.map addrs ~f:Addr.of_string in
          let locs = Locations.create ~prev:addrs addr in
          let inc_kind = Incident.Kind.of_string kind in
          let m = match String.lowercase rel with
            | "must" ->
               let cnf = Confirmation.(create must inc_kind locs) in
               Map.add_multi m arti cnf
            | "may"  ->
               let cnf = Confirmation.(create may inc_kind locs) in
               Map.add_multi m arti cnf
            | _ -> m in
          parse m lines
       | _ -> parse m lines in
  Map.to_alist @@
    parse (Map.empty (module String)) (In_channel.input_lines ch)
