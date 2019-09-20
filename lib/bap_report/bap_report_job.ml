open Core_kernel
open Bap_report_utils
open Bap_report_types
open Bap_report_read

module Docker = Bap_report_docker
module Recipe = Bap_report_recipe

type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image


let drive = "/mydrive"
let pwd = Sys.getcwd
let mytime = "mytime"

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
  incidents : incident list;
  time : float;
  errors : string list list;
}

let time t = t.time

let script dir target recipe limit =
  let recipe = Recipe.to_string recipe in
  let ulimit = match Limit.ulimit limit with
    | None -> ""
    | Some cmd -> cmd in
  [ "#!/usr/bin/env sh\n";
    sprintf "cd %s" drive;
    sprintf "mkdir %s" dir;
    sprintf "cd %s" dir;
    sprintf "%s" ulimit;
    sprintf
      "/usr/bin/time -v -o %s bap %s/%s --recipe=%s -d -dasm > bap.stdout" mytime drive target recipe;
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

let read_tar ?target_dir target_file tar read =
  if Sys.file_exists tar then
    let dir = Filename.remove_extension tar in
    let path = match target_dir with
      | None -> sprintf "%s/%s" dir target_file
      | Some dir' -> sprintf "%s/%s/%s" dir dir' target_file in
    let _ = cmd "tar xzf %s %s" tar path in
    if Sys.file_exists path then
      let r = read path in
      Sys.remove path;
      Option.iter target_dir ~f:(fun dir' -> Unix.rmdir (sprintf "%s/%s" dir dir'));
      Unix.rmdir dir;
      r
    else None
  else None

let read_incidents file =
  let read f = Some (In_channel.with_file f ~f:Bap_report_read.incidents) in
  match read_tar "incidents" file read with
  | None -> []
  | Some incs -> incs

let read_log file =
  read_tar ~target_dir:"log" "log" file Bap_log.of_file |> function
  | Some log -> Bap_log.errors log
  | None -> []

let run recipe ~tool ?image ?(limit=Limit.empty) path =
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target ?image ~path (Filename.basename alias);
  let workdir = workdir ?image path (Recipe.name recipe) in
  let entry = entry workdir (Filename.basename alias) recipe limit in
  let start = Unix.gettimeofday () in
  let _ = Docker.run tool ~mount:(pwd (), drive)
      ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) "" in
  let finish = Unix.gettimeofday () in
  Sys.remove alias;
  Sys.remove entry;
  let file = workdir ^ ".tgz" in
  let incidents = read_incidents file in
  let errors = read_log file in
  { incidents; time = finish -. start; errors }

let incidents t = t.incidents
let errors t = t.errors
