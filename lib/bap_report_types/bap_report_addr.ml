open Core_kernel

include String

let addr_of_string str =
  match String.split str ~on:':' with
  | x :: _ ->
    let prefix = "0x" in
    let x =
      if String.is_prefix x ~prefix:"0x" then
        String.chop_prefix_exn x ~prefix
      else x in
    prefix ^ String.lowercase x
  | _ -> str


let to_string t = t

let of_string = addr_of_string
