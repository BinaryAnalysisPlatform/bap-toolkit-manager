open Core_kernel
open Bap_report_utils

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
}

type recipe = t

let drive = "/mydrive"
let pwd = Sys.getcwd

let name t = t.name
let description t = t.desc

let list tool =
  let recipe_of_string s =
    match split_on_first ~on:[' '; '\t'] s with
    | name :: desc :: _ ->
      let name = String.strip name in
      let desc = String.strip desc in
      Some {name;desc;args=[]}
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


module Job = struct

  module Limit = struct
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
      sprintf "-v %d" n

    let ulimit t =
      let map ~f =
        Option.value_map ~default:None ~f:(fun x -> Some (f x)) in
      let time = map ~f:string_of_time t.time in
      let mem  = map ~f:string_of_mem t.mem in
      match List.filter_map ~f:ident [time;mem] with
      | [] -> None
      | args ->
         Some (sprintf "ulimit %s" @@ String.concat ~sep:" " args)

  end

  type limit = Limit.t


  type t = {
    file : string;
    time : float;
  }

  let time t = t.time
  let results t = t.file

  let script dir target recipe limit =
    let recipe = to_string recipe in
    let ulimit = match Limit.ulimit limit with
      | None -> ""
      | Some cmd -> cmd in
    [ "#!/usr/bin/env sh\n";
      sprintf "cd %s" drive;
      sprintf "mkdir %s" dir;
      sprintf "cd %s" dir;
      sprintf "%s" ulimit;
      sprintf "bap %s/%s --recipe=%s -d -dasm > bap.stdout" drive target recipe;
      "cd ../";
      sprintf "tar czf %s.tgz %s" dir dir;
      sprintf "rm -r %s" dir
    ] |> String.concat ~sep:"\n"

  let entry workdir target recipe limit =
    let s = script workdir target recipe limit in
    let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
    Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [s]);
    Unix.chmod tmp 0o777;
    tmp

  let copy_target ?image ~path alias =
    ignore @@
    match image with
    | None -> cmd "cp %s %s/%s" path (pwd ()) alias
    | Some image ->
      Docker.run ~mount:(pwd (), drive) image
      @@ sprintf "cp %s %s/%s" path drive alias

  let workdir ?image path recipe =
    match image with
    | None -> sprintf "%s.%s" (Filename.basename path) recipe
    | Some im ->
      match Docker.Image.tag im with
      | None ->
        sprintf "%s.%s" (Docker.Image.to_string im) recipe
      | Some tag -> sprintf "%s.%s" tag recipe

  let run t ~tool ?image ?(limit=Limit.empty) path =
    let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
    copy_target ?image ~path (Filename.basename alias);
    let workdir = workdir ?image path t.name in
    let entry = entry workdir (Filename.basename alias) t limit in
    let start = Unix.gettimeofday () in
    let _ = Docker.run tool ~mount:(pwd (), drive)
        ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) "" in
    let finish = Unix.gettimeofday () in
    Sys.remove alias;
    Sys.remove entry;
    { file=workdir ^ ".tgz"; time = finish -. start }

  let incidents {file} =
    let dir = Filename.remove_extension file in
    let incs_file = sprintf "%s/incidents" dir in
    let _ = cmd "tar xzf %s %s" file incs_file in
    if Sys.file_exists incs_file then
      let incs = In_channel.with_file incs_file ~f:Bap_report_read.incidents in
      Sys.remove incs_file;
      Unix.rmdir dir;
      incs
    else []

end
