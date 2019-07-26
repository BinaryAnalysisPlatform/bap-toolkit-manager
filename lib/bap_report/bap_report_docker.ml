open Core_kernel
open Bap_report_types


let pwd = Sys.getcwd
let drive = "/mydrive"
let toolkit = "binaryanalysisplatform/bap-toolkit"
let artifacts = "binaryanalysisplatform/bap-artifacts"


exception Command_failed of Unix.process_status

let cmd fmt =
  let run c =
    try
      let inp = Unix.open_process_in c in
      let res = In_channel.input_all inp in
      match Unix.close_process_in inp with
      | Unix.WEXITED 0 -> Some res
      | s -> raise (Command_failed s)
    with e ->
      eprintf "command %s failed: %s\n" c (Exn.to_string e);
      None in
  ksprintf run fmt

let image_exists ?tag image =
  let r = match tag with
    | None -> cmd "docker images | grep %s" image
    | Some t -> cmd "docker images | grep %s | grep %s" image t in
  match r with
  | None | Some "" -> false
  | _ -> true

let artifact_image_exists name =
  image_exists artifacts ~tag:name

let image_pull ?tag image =
  ignore @@
    match tag with
    | None -> cmd "docker pull %s" image
    | Some t -> cmd "docker pull %s:%s" image t

let pull_artifact name = image_pull artifacts ~tag:name

let size kind arti =
  let size =
    match kind with
    | Artifact.Virtual ->
       cmd "docker run -ti %s:%s stat -c%%s /artifact" artifacts arti
    | Artifact.Physical -> cmd "stat -c%%s %s" arti in
  match size with
  | None -> None
  | Some s ->
     let s = String.strip s in
     try
       Some (int_of_string s)
     with _ -> None

let script arti recipe =
  sprintf
"#!/usr/bin/env sh

bap %s/%s --recipe=%s

if [ -f incidents ]; then
   cp incidents %s
fi" drive arti recipe drive

let prepare_entry arti recipe =
  let s = script arti recipe in
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [s]);
  Unix.chmod tmp 0o777;
  tmp

let cmd_of_strings = function
  | [] -> Bos.Cmd.empty
  | fst :: others ->
     List.fold others ~init:(Bos.Cmd.v fst) ~f:Bos.Cmd.(%)

let copy_artifact arti alias =
  let name = Artifact.name arti in
  match Artifact.kind arti with
  | Some Artifact.Physical ->
     Option.some @@
     cmd_of_strings [
         "cp"; name;
         sprintf "%s/%s" (pwd ()) alias
       ]
  | Some Artifact.Virtual ->
     Option.some @@
     cmd_of_strings [
         "docker"; "run"; "-ti"; "-v";
         sprintf "%s:%s" (pwd ()) drive;
         sprintf "binaryanalysisplatform/bap-artifacts:%s" name;
         "cp"; "/artifact";
         sprintf "%s/%s" drive alias
       ]
  | None -> None

let run_script script =
  cmd_of_strings [
      "docker"; "run"; "-ti"; "-v";
      sprintf "%s:%s" (pwd ()) drive;
      "--entrypoint";
      sprintf "%s/%s" drive script;
      "binaryanalysisplatform/bap-toolkit"
    ]

type t = {
    time : float;
  }

let run_recipe artifact recipe =
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "arti" "" in
  let tmp' = Filename.basename tmp in
  let script = prepare_entry tmp' recipe in
  let script' = Filename.basename script in
  match copy_artifact artifact tmp' with
  | None -> None
  | Some cmd ->
     let _ = Bos.OS.Cmd.run cmd in
     let start = Unix.gettimeofday () in
     let _ = Bos.OS.Cmd.run (run_script script') in
     let finish = Unix.gettimeofday () in
     Sys.remove tmp;
     Sys.remove script;
     Some { time = finish -. start }

let time_taken t = t.time

let find_artifact name =
  let create kind =
    let size = size kind name in
    Some (Artifact.create ?size ~kind name) in
  if Sys.file_exists name && not (Sys.is_directory name) then
    create Artifact.Physical
  else
    let kind = Artifact.Virtual in
    if artifact_image_exists name then create kind
    else
      let () = pull_artifact name in
      if artifact_image_exists name then create kind
      else
        let () = eprintf "can't find/pull artifact %s\n" name in
        None

let toolkit_exists () =
  if not (image_exists toolkit) then
    let () = image_pull toolkit in
    image_exists toolkit
  else true
