open Core_kernel
open Bap_report_utils
open Bap_report_types
open Bap_report_read
open Bap_report_env

module Docker  = Bap_report_docker
module Recipe  = Bap_report_recipe
module Script  = Bap_report_script
module Journal = Script.Journal
module Limit   = Bap_report_limit
module Env     = Bap_report_env
module Path    = Bap_report_path
module Tool    = Bap_report_tool

type tool = Bap_report_tool.t
type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image
type limit = Bap_report_limit.t
type path = Bap_report_path.t


let pwd = Sys.getcwd

type t = {
  incidents : incident list;
  time : float;
  errors : string list;
}

type ctxt = {
  tool    : tool;
  limit   : limit;
  verbose : bool;
}

let incidents t = t.incidents
let errors t = t.errors
let time t = t.time

let entry script =
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [script]);
  Unix.chmod tmp 0o777;
  tmp

let copy_target path alias =
  ignore @@
  match Path.image path with
  | None -> cmd "cp %s %s/%s" (Path.relative path) (pwd ()) alias
  | Some image ->
    Docker.run ~mount:(pwd (), drive) image
    @@ sprintf "cp %s %s/%s" (Path.relative path) drive alias

let workdir path recipe =
  match Path.image path with
  | None -> sprintf "%s.%s" (Filename.basename @@ Path.relative path) recipe
  | Some im ->
    match Docker.Image.tag im with
    | None -> sprintf "%s.%s" (Docker.Image.to_string im) recipe
    | Some tag -> sprintf "%s.%s" tag recipe

let context ?(verbose=true) ?(limit=Limit.empty) tool = {verbose; limit; tool}

let apply tool entry =
  match Tool.image tool with
  | Some im ->
     ignore @@
       Docker.run im ~mount:(pwd (), drive)
         ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) ""
  | None -> ignore @@ cmd "sh %s" entry

let run {verbose; tool; limit} recipe path =
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target path (Filename.basename alias);
  let workdir = workdir path (Recipe.name recipe) in
  let script,journal =
    let pwd =
      if Option.is_none (Tool.image tool) then Sys.getcwd()
      else Env.drive in
    Script.create ~limit ~pwd ~verbose ~workdir
      ~path:(Filename.basename alias) recipe in
  let entry = entry script in
  let start = Unix.gettimeofday () in
  apply tool entry;
  let finish = Unix.gettimeofday () in
  Sys.remove alias;
  Sys.remove entry;
  let time = Option.value ~default:(finish -. start) (Journal.time journal) in
  let incidents = Journal.incidents journal in
  let errors = Journal.errors journal in
  { incidents; time; errors }
