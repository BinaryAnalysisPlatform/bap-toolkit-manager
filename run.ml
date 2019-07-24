open Core_kernel

let pwd = Sys.getcwd

let size arti =
  let cmd = sprintf "docker run -ti binaryanalysisplatform/bap-artifacts:%s stat -c%%s /artifact" arti in
  let inp = Unix.open_process_in cmd in
  let str = In_channel.input_line inp in
  let _ = Unix.close_process_in inp in
  match str with
  | None -> None
  | Some str ->
     let bytes = int_of_string str in
     let kbytes = bytes / 1024 in
     let mbytes = kbytes / 1024 in
     let hum =
       match mbytes, kbytes, bytes with
       | 0,0,b -> sprintf "%d bytes" b
       | 0,k,_ -> sprintf "%dK" k
       | m,_,_ -> sprintf "%dM" m in
     Some hum

let drive = "/mydrive"

let script arti recipe =
  let s =
    sprintf  "#!/usr/bin/env sh

bap %s/%s --recipe=%s

if [ -f incidents ]; then
  cp incidents %s
fi
" drive arti recipe drive in
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "script" "" in
  Out_channel.with_file tmp ~f:(fun c -> Out_channel.output_lines c [s]);
  Unix.chmod tmp 0o777;
  tmp

let copy_artifact arti alias =
  Bos.Cmd.(v "docker" % "run" % "-ti" % "-v" %
           sprintf "%s:%s" (pwd ()) drive %
           sprintf "binaryanalysisplatform/bap-artifacts:%s" arti %
           "cp" % "/artifact" % sprintf "%s/%s" drive alias)

let run_script script =
  Bos.Cmd.(v "docker" % "run" % "-ti" % "-v" %
           sprintf "%s:%s" (pwd ()) drive %
           "--entrypoint" %
           sprintf "%s/%s" drive script %
           "binaryanalysisplatform/bap-toolkit" )

let run_recipe artifact recipe =
  let tmp = Filename.temp_file ~temp_dir:(pwd ()) "arti" "" in
  let tmp' = Filename.basename tmp in
  let script = script tmp' recipe in
  let script' = Filename.basename script in
  let _ = Bos.OS.Cmd.run (copy_artifact artifact tmp') in
  let _ = Bos.OS.Cmd.run (run_script script') in
  Sys.remove tmp;
  Sys.remove script

let run artifacts recipes =
  let output = "summary" in
  let () = Out_channel.with_file output ~f:(fun c -> ()) in
  List.iter artifacts ~f:(fun a ->
      List.iter recipes ~f:(fun r ->
          run_recipe a r;
          Bap_report_parse_incidents.process ~output ~name:a "incidents";
        ))

let () =
  let artifacts = ["test-only"; "test-only-2";] in
  let recipes = ["defective-symbol"; "av-rule-174"; "jpl-rule-14"] in
  run artifacts recipes
