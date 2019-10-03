open Core_kernel
open Bap_report_utils
open Bap_report_types
open Bap_report_read
open Bap_report_env

module Docker = Bap_report_docker
module Recipe = Bap_report_recipe
module Script = Bap_report_script
module Limit  = Bap_report_limit
module Env    = Bap_report_env

type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image
type limit = Bap_report_limit.t

let pwd = Sys.getcwd

type t = {
  incidents : incident list;
  time : float;
  errors : string list;
}

let time t = t.time

let entry workdir path recipe limit verbose =
  let s = Script.create ~limit ~verbose ~workdir ~path recipe in
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

let tar_list tar =
  match cmd "tar -ztf %s" tar with
  | None -> []
  | Some s ->
     List.filter_map ~f:(fun x ->
         let x = String.strip x in
         if String.is_empty x then None
         else Some x) @@
       String.split ~on:'\n' s

let tar_exists ~file tar =
  List.exists (tar_list tar) ~f:(fun s -> String.equal s file)

let read_tar ?target_dir target_file tar read =
  if Sys.file_exists tar then
    let dir = Filename.remove_extension tar in
    let path = match target_dir with
      | None -> sprintf "%s/%s" dir target_file
      | Some dir' -> sprintf "%s/%s/%s" dir dir' target_file in
    if tar_exists ~file:path tar then
      let _ = cmd "tar xzf %s %s" tar path in
      if Sys.file_exists path then
        let r = read path in
        Sys.remove path;
        Option.iter target_dir ~f:(fun dir' -> Unix.rmdir (sprintf "%s/%s" dir dir'));
        Unix.rmdir dir;
        r
      else None
    else None
  else None

let read_incidents tar =
  let read f = Some (In_channel.with_file f ~f:Bap_report_read.incidents) in
  match read_tar "incidents" tar read with
  | None -> []
  | Some incs -> incs

let read_errors tar =
  match read_tar ~target_dir:"log" "log" tar Log.of_file with
  | None -> []
  | Some log -> Log.errors log

let read_time ~default tar =
  match read_tar Env.mytime tar Time.of_file with
  | None -> default
  | Some tm -> Option.value ~default (Time.elapsed tm)

let run recipe ~tool ?(verbose=true) ?image ?(limit=Limit.empty) path =
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target ?image ~path (Filename.basename alias);
  let workdir = workdir ?image path (Recipe.name recipe) in
  let entry = entry workdir (Filename.basename alias) recipe limit verbose in
  let start = Unix.gettimeofday () in
  let _ = Docker.run tool ~mount:(pwd (), drive)
      ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) "" in
  let finish = Unix.gettimeofday () in
  Sys.remove alias;
  Sys.remove entry;
  let file = workdir ^ ".tgz" in
  let time = read_time ~default:(finish -. start) file in
  let incidents = read_incidents file in
  let errors = read_errors file in
  { incidents; time; errors }

let incidents t = t.incidents
let errors t = t.errors
