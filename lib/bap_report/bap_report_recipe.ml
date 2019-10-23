open Core_kernel
open Bap_report_utils
open Bap_report_types

module Filename = Caml.Filename
module Docker = Bap_report_docker

type image = Bap_report_docker.image

let split_string s =
  String.split_on_chars ~on:[' '; '\t'] s |>
  List.filter ~f:(fun s -> s <> "") |>
  List.map ~f:String.strip

let split_on_first s ~on =
  let indexes = List.filter_map on ~f:(String.index s) in
  let indexes = List.sort ~compare:Int.compare indexes in
  match indexes with
  | [] -> [s]
  | i :: _ ->
    [String.subo ~len:i s;
     String.subo ~pos:(i + 1) s]

type t =  {
  name : string;
  desc : string;
  args : (string * string) list;
  kinds : incident_kind list;
}

type recipe = t

let name t = t.name
let description t = t.desc

let create ~name ~desc =
  {name;desc;args=[];kinds=[]}

let list tool =
  let recipe_of_string s =
    match split_on_first ~on:[' '; '\t'] s with
    | name :: desc :: _ ->
      let name = String.strip name in
      let desc = String.strip desc in
      Some {name;desc;args=[];kinds=[]}
    | _ -> None in
  match Docker.run tool "--list-recipes" with
  | None | Some "" -> []
  | Some r ->
    let rs = String.split ~on:'\n' r in
    List.filter_map rs ~f:recipe_of_string

let find tool name' =
  List.find (list tool) ~f:(fun {name} -> String.equal name name')

let add_parameter t ~name ~value =
  { t with args = (name,value) :: t.args }

let to_string recipe = match recipe.args with
  | [] -> recipe.name
  | args ->
    let args = List.map args ~f:(fun (a,v) -> sprintf "%s=%s" a v) in
    let args = String.concat ~sep:"," args in
    sprintf "%s:%s" recipe.name args


let provide t k = {t with kinds = k :: t.kinds}
let kinds t = t.kinds
