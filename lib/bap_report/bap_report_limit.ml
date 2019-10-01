open Core_kernel

type mem_quantity  = [ `Mb | `Gb ]
type time_quantity = [ `S | `M | `H ]
type quantity = [ time_quantity | mem_quantity ]

type time = int * time_quantity
type mem  = int * mem_quantity

type t = {
  time : time option;
  mem  : mem option
}

let empty = {time = None; mem = None}

let quantity_of_string = function
  | "s" -> Some `S
  | "m" -> Some `M
  | "h" -> Some `H
  | "Mb" -> Some `Mb
  | "Gb" -> Some `Gb
  | _ -> None

let string_of_quantity = function
  | `S -> "s"
  | `M -> "m"
  | `H -> "h"
  | `Mb -> "Mb"
  | `Gb -> "Gb"


let add t num quantity  =
  match quantity with
  | `Mb | `Gb as quantity -> {t with mem  = Some (num,quantity)}
  | `S | `M | `H  as quantity -> {t with time  = Some (num,quantity)}

let time t = t.time
let memory t = t.mem

let string_of_time (n,quantity) =
  let n = match quantity with
    | `S -> n
    | `M -> n * 60
    | `H -> n * 60 * 60 in
  sprintf "-t %d" n

let string_of_mem (n,quantity) =
  let n = match quantity with
    | `Mb -> 1024 * n
    | `Gb -> 1024 * 1024 * n in
  sprintf "-m %d" n

let string_of_t t =
  let map ~f =
    Option.value_map ~default:None ~f:(fun x -> Some (f x)) in
  let time = map ~f:string_of_time t.time in
  let mem  = map ~f:string_of_mem t.mem in
  match List.filter_map ~f:ident [time;mem] with
  | [] -> ""
  | args ->
    sprintf "ulimit %s" @@ String.concat ~sep:" " args
