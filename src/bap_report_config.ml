open Core_kernel
open Bap_report.Std

open View

type t = unit

let providers = String.Table.create ()
let empty = ()

let read_sexp ch =
  try Sexp.input_sexp ch |> Option.some
  with _ -> None

let get_alias data =
  match data with
  | [Sexp.Atom x] -> [Alias x]
  | _ -> []

let get_web data =
  match data with
  | [Sexp.Atom x] -> [Web x]
  | _ -> []

let get_tab data =
  try
    let cols = List.map data ~f:col_of_sexp in
    List.map cols ~f:(fun c -> Col c)
  with _ -> []

let update_view kind data f =
  List.iter (f data) ~f:(register kind)

let update_providers kind data =
  let rec get acc = function
    | Sexp.Atom x -> x :: acc
    | Sexp.List xs ->
      List.fold xs ~init:acc ~f:get in
  let ps = List.fold data ~init:[] ~f:get in
  List.iter ps ~f:(fun p ->
      Hashtbl.update providers p ~f:(function
          | None -> [kind]
          | Some ks -> kind :: ks))

let update = function
  | Sexp.(List (Atom s :: Atom kind :: data)) ->
    begin
      let kind = Incident.Kind.of_string kind in
      let update_view = update_view kind data in
      match s with
      | "alias" -> update_view get_alias
      | "web"   -> update_view get_web
      | "tab"   -> update_view get_tab
      | "provides" -> update_providers kind data
      | _ -> ()
    end
  | _ -> ()

let add_info str =
  try
    Sexp.of_string str |> update
  with _ -> ()

module Read = Bap_report_read.Helper

let read ch =
  let rec loop = function
    | [] -> ()
    | line :: lines ->
      let line = sprintf "(%s)" line in
      add_info line;
      loop lines in
  loop (Read.lines ~comments:"#" ch)

let read fname = In_channel.with_file fname ~f:read

let provide_kinds () recipe =
  match Hashtbl.find providers (Recipe.name recipe) with
  | None -> recipe
  | Some kinds ->
     List.fold kinds ~init:recipe ~f:Recipe.provide
