open Core_kernel
open Bap_report_utils

module Docker = Bap_report_docker

let size ?image ?tag path =
  let size = match image with
    | Some image ->
       Docker.run ~image ?tag (sprintf "stat -c%%s %s" path)
    | None -> cmd "stat -c%%s %s" path in
  match size with
  | None -> None
  | Some s ->
     let s = String.strip s in
     try Some (int_of_string s)
     with _ -> None
