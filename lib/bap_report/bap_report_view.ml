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
  | Tab of col list
  | Alias of string
[@@deriving sexp]


type t = info list String.Table.t

let create () = Hashtbl.create (module String)

let update t k info =
  Hashtbl.update t (Incident.Kind.to_string k) ~f:(function
      | None -> [info]
      | Some xs -> info :: xs)

let find_info t k ~f =
  match Hashtbl.find t (Incident.Kind.to_string k) with
  | None -> None
  | Some infos -> List.find_map infos ~f

let name t kind =
  match find_info t kind ~f:(function
            | Alias a -> Some a
            | _ -> None) with
  | None -> Incident.Kind.to_string kind
  | Some a -> a

let path t inc =
  match find_info t (Incident.kind inc) ~f:(function
      | Path p -> Some p
      | _ -> None) with
  | None -> []
  | Some n ->
     List.take (List.rev @@ Incident.path inc) n

let web t inc =
  find_info t inc ~f:(function
      | Web w -> Some w
      | _ -> None)

let symbol inc =
  match Incident.path inc with
  | [] -> None
  | a :: _ -> Some a


let data t inc =
  let kind = Incident.kind inc in
  let cols = find_info t kind ~f:(function
      | Tab cols -> Some cols
      | _ -> None) in
  let cols = match cols with
    | None -> [Name; Addr]
    | Some cols -> cols in
  List.rev @@
    List.fold cols ~init:[] ~f:(fun acc -> function
        | Path n ->
           (List.rev (List.take (Incident.path inc) n)) @ acc
        | Name ->
           let name = match symbol inc with
             | None -> name t kind
             | Some sym -> sym in
           name :: acc
        | Addr -> Addr.to_string (Incident.addr inc) :: acc
        | Locations ->
           let addrs = Locations.addrs (Incident.locations inc) in
           List.rev_map ~f:Addr.to_string addrs @ acc)

let read_sexp ch =
  try Sexp.input_sexp ch |> Option.some
  with _ -> None

let get_alias data =
  match data with
  | [Sexp.Atom x] -> Some (Alias x)
  | _ -> None

let get_web data =
  match data with
  | [Sexp.Atom x] -> Some ( Web x)
  | _ -> None

let get_tab data =
  try
    Some (Tab (List.map data ~f:col_of_sexp))
  with _ -> None

let info_of_sexp s =
  let open Sexp in
  match s with
  | List (Atom s :: Atom kind :: data) ->
     let data =  match s with
       | "alias" -> get_alias data
       | "web"   -> get_web data
       | "tab"   -> get_tab data
       | _ -> None in
     Option.value_map data ~default:None ~f:(fun x -> Some (kind,x))
  | _ -> None

let info_of_str str =
  try
    Sexp.of_string str |> info_of_sexp
  with _ -> None


let read ch =
  let t = create () in
  let rec loop ()  =
    match In_channel.input_line ch with
    | None -> ()
    | Some line when String.is_prefix ~prefix:"#" line -> loop ()
    | Some s ->
       match info_of_str s with
       | None -> loop ()
       | Some (kind,info) ->
          update t (Incident.Kind.of_string kind) info;
          loop () in
  loop ();
  t

let of_file fname = In_channel.with_file fname ~f:read
