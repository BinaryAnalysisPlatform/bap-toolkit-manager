open Core_kernel
open Bap_report_types
open Bap_report_utils

module Recipe = Bap_report_recipe

type recipe = Bap_report_recipe.t

type t =
  | Image of image
  | Host

let bap_version = function
  | Image im -> Image.run im "--version"
  | Host -> cmd "bap --version"

let check t =
  match bap_version t with
  | None -> Error (Error.of_string "bap not found!")
  | Some _ -> Ok t

let of_image im = check (Image im)
let host () = check Host

let split_on_first s ~on =
  let indexes = List.filter_map on ~f:(String.index s) in
  let indexes = List.sort ~compare:Int.compare indexes in
  match indexes with
  | [] -> [s]
  | i :: _ ->
    [String.subo ~len:i s;
     String.subo ~pos:(i + 1) s]

let recipes_of_string s =
  let recipe_of_string s =
    match split_on_first ~on:[' '; '\t'] s with
    | name :: desc :: _ ->
       let name = String.strip name in
       let desc = String.strip desc in
       Some (Recipe.create ~name ~desc)
    | _ -> None in
  let rs = String.split ~on:'\n' s in
  List.filter_map rs ~f:recipe_of_string

let recipes tool =
  let str = match tool with
    | Host -> cmd "bap --list-recipes"
    | Image im -> Image.run im "--list-recipes" in
  match str with
  | None | Some "" -> []
  | Some s -> recipes_of_string s

let find_recipe tool recipe =
  List.find (recipes tool)
    ~f:(fun r -> String.equal (Recipe.name r) recipe)

let image = function
  | Image im -> Some im
  | Host -> None

let to_string  = function
  | Host -> "host"
  | Image im -> Image.to_string im
