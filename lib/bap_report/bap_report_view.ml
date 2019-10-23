open Core_kernel
open Bap_report_types

type col =
  | Path of int (* deep  *)
  | Name
  | Addr
  | Locations
[@@deriving sexp]

type info =
  | Web of string
  | Col of col
  | Alias of string
[@@deriving sexp]

let view = Hashtbl.create (module Incident.Kind)

let register k info =
  Hashtbl.update view k ~f:(function
      | None -> [info]
      | Some xs -> info :: xs)

let find_info k ~f =
  match Hashtbl.find view k with
  | None -> None
  | Some infos -> List.find_map infos ~f

let name kind =
  match find_info kind ~f:(function
      | Alias a -> Some a
      | _ -> None) with
  | None -> Incident.Kind.to_string kind
  | Some a -> a

let web inc =
  find_info inc ~f:(function
      | Web w -> Some w
      | _ -> None)

let symbol inc =
  match Incident.path inc with
  | [] -> None
  | a :: _ -> Some a

let default_cols = [Name;Addr]

let find_tab k =
  match Hashtbl.find view k with
  | None -> default_cols
  | Some infos ->
    List.rev @@
    List.filter_map infos ~f:(function
        | Col c -> Some c
        | _ -> None) |> function
    | [] -> default_cols
    | cs -> cs

let tab_of_incident inc =
  let kind = Incident.kind inc in
  let cols = find_tab kind in
  List.rev @@
  List.fold cols ~init:[] ~f:(fun acc -> function
      | Path n ->
        (List.rev (List.take (Incident.path inc) n)) @ acc
      | Name ->
        let name = match symbol inc with
          | None -> name kind
          | Some sym -> sym in
        name :: acc
      | Addr -> Addr.to_string (Incident.addr inc) :: acc
      | Locations ->
        let addrs = Locations.addrs (Incident.locations inc) in
        List.rev_map ~f:Addr.to_string addrs @ acc)
