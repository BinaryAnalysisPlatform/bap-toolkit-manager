open Core_kernel
open Bap_report_docker
open Bap_report_utils

type t = image option * string

let create ?image path = image, path

let image = fst
let relative = snd

let size (image,path) =
  let size = match image with
    | Some image ->
      run image (sprintf "stat -c%%s %s" path)
    | None -> cmd "stat -c%%s %s" path in
  match size with
  | None -> None
  | Some s ->
    let s = String.strip s in
    try Some (int_of_string s)
    with _ -> None
