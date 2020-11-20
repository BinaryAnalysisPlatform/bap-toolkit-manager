open Core_kernel
open Bap_report_utils

type t = string * string option
[@@deriving bin_io, compare, sexp]

let split_string s =
  String.split_on_chars ~on:[' '; '\t'] s |>
  List.filter ~f:(fun s -> String.(s <> "")) |>
  List.map ~f:String.strip

module type A = sig
  val tags : string -> string list
  val image : string -> bool
end

module Net_available = struct

  let tags image =
    match cmd "wget -q https://registry.hub.docker.com/v1/repositories/%s/tags -O -" image with
    | None -> []
    | Some s ->
      let s = String.filter s ~f:(fun c -> Char.(c <> '"')) in
      let s = String.tr s ~target:'}' ~replacement:' ' in
      let s = String.split ~on:' ' s in
      fst @@
      List.fold s ~init:([],false)
        ~f:(fun (acc,remember) w ->
            if remember then w :: acc, false
            else if String.(w = "name:") then acc,true
            else acc,false)

  let image name =
    let name' =
      match String.split name ~on:'/' with
      | [_repo;name] -> name
      | _ -> name in
    match cmd "docker search %s" name' with
    | None -> false
    | Some s ->
      match String.split ~on:'\n' s with
      | _header :: names ->
        let names = List.filter_map names
            ~f:(fun s -> split_string s |> List.hd) in
        List.mem names name ~equal:String.equal
      | _ -> false

end

module Loc_available = struct

  let available () =
    match cmd "docker images" with
    | None | Some "" -> []
    | Some r ->
      match String.split ~on:'\n' r with
      | [] | [_] -> []
      | _header :: images ->
        List.filter_map images ~f:(fun im ->
            match split_string im with
            | name :: tag :: _ ->
              let tag = match tag with
                | "<none>" -> None
                | tag -> Some tag in
              Some (name,tag)
            | _ -> None)

  let tags image =
    List.fold (available ()) ~init:[] ~f:(fun acc (im,tag) ->
        if String.equal image im then
          match tag with
          | None -> acc
          | Some tag -> tag :: acc
        else acc)

  let image name =
    List.exists (available ()) ~f:(fun (im,_) -> String.equal name im)

end

module Exists(A : A) = struct
  let test (name,tag) =
    match tag with
    | None -> A.image name
    | Some tag -> List.mem (A.tags name) tag ~equal:String.equal
end

let exists_locally image =
  let module E = Exists(Loc_available) in
  E.test image

let exists_globaly image =
  let module E = Exists(Net_available) in
  E.test image

let exists image = exists_locally image || exists_globaly image

let to_string (name,tag) =
  match tag with
  | None -> name
  | Some tag -> sprintf "%s:%s" name tag

let pull image =
  let image = to_string image in
  cmd "docker pull %s" image |> ignore

let get image =
  if exists_locally image then Ok ()
  else
  if exists_globaly image then
    let () = pull image in
    if exists_locally image then Ok ()
    else
      Or_error.errorf "can't pull image %s" (to_string image)
  else Or_error.errorf "can't detect image %s" (to_string image)

let check name  =
  match String.split name ~on:':' with
  | [name;tag] -> Ok (name, Some tag)
  | [name] -> Ok (name, None)
  | _ -> Or_error.errorf "can't infer image name from the %s" name

let (>>=) = Or_error.(>>=)

let of_string s =
  check s >>= fun im  ->
  get im >>= fun () ->
  Ok im

let of_string_exn s =
  match of_string s with
  | Ok im -> im
  | Error e -> raise (Invalid_argument (Error.to_string_hum e))

let tag (_,tag) = tag
let name (name,_) = name

let with_tag (name,_) tag = name, Some tag

let tags (image,_) =
  match Net_available.tags image with
  | [] -> Loc_available.tags image
  | tags -> tags

let run ?entry ?mount image cmd' =
  let image = to_string image in
  let mount = match mount with
    | None -> ""
    | Some (host,guest) -> sprintf "-v %s:%s" host guest in
  let entry = match entry with
    | None -> ""
    | Some e -> sprintf "--entrypoint %s" e in
  cmd "docker run --rm -ti %s %s %s %s" mount entry image cmd'
