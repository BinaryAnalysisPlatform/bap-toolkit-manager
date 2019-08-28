open Core_kernel
open Bap_report_utils


module Filename = Caml.Filename
module Docker = Bap_report_docker

module Tool = Bap_report_tool

type tool = Tool.t

let with_tag ?tag image =
  match tag with
  | None -> image
  | Some tag -> sprintf "%s:%s" image tag

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
    time : float;
  }


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
       Some {name;desc;time = 0.0}
    | _ -> None in
  let tag = Tool.tag tool in
  let image = Tool.name tool in
  match Docker.run ?tag ~image "--list-recipes" with
  | None | Some "" -> []
  | Some r ->
     let rs = String.split ~on:'\n' r in
     List.filter_map rs ~f:recipe_of_string

let find tool name' =
  List.find (list tool) ~f:(fun {name} -> String.equal name name')

let time_taken {time} = time

let script target recipe =
  sprintf
"#!/usr/bin/env sh

bap %s/%s --recipe=%s

if [ -f incidents ]; then
   cp incidents %s
fi" drive target recipe drive

let entry target recipe =
  let s = script target recipe in
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [s]);
  Unix.chmod tmp 0o777;
  tmp

let copy_target ?image ?tag ~path alias =
  ignore @@
    match image with
    | None -> cmd "cp %s %s/%s" path (pwd ()) alias
    | Some image ->
       Docker.run ~mount:(pwd (), drive) ~image:(with_tag ?tag image)
       @@ sprintf "cp %s %s/%s" path drive alias

let run ~tool ?image ?tag path t =
  let alias = Filename.temp_file ~temp_dir:(pwd ()) "artifact" "" in
  copy_target ?image ?tag ~path (Filename.basename alias);
  let entry = entry (Filename.basename alias) t.name in
  let start = Unix.gettimeofday () in
  let tag = Tool.tag tool in
  let image = Tool.name tool in
  let _ = Docker.run ?tag ~image ~mount:(pwd (), drive)
            ~entry:(sprintf "%s/%s" drive @@ Filename.basename entry) "" in
  let finish = Unix.gettimeofday () in
  Sys.remove alias;
  Sys.remove entry;
  {t with time = finish -. start}
