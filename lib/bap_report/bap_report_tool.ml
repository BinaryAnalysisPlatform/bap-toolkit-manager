open Core_kernel
open Bap_report_utils

type t = string * string option

let check name  =
  match String.split name ~on:':' with
  | [name;tag] -> Ok (name, Some tag)
  | [name] -> Ok (name, None)
  | _ -> Or_error.errorf "can't infer image name from the %s" name

let of_string name =
  let (>>=) = Or_error.(>>=) in
  check name >>= fun (name,tag) ->
  Ok (name,tag)

let tag (_,tag) = tag

let name (name,_) = name
