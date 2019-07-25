open Core_kernel
open Bap_report_types

module Template = Bap_report_template

module Docker = struct

  let pwd = Sys.getcwd

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

  let image_exists arti =
    match
      cmd "docker images | grep binaryanalysisplatform/bap-artifacts | grep %s" arti
    with
    | None
    | Some "" -> false
    | _ -> true

  let pull arti =
    ignore @@
    cmd "docker pull binaryanalysisplatform/bap-artifacts:%s" arti

  let size arti =
    match
      cmd "docker run -ti binaryanalysisplatform/bap-artifacts:%s stat -c%%s /artifact" arti
    with
    | None -> None
    | Some s ->
       try
         Some (int_of_string s)
       with _ -> None

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
    let artifact = Artifact.name artifact in
    let tmp = Filename.temp_file ~temp_dir:(pwd ()) "arti" "" in
    let tmp' = Filename.basename tmp in
    let script = script tmp' recipe in
    let script' = Filename.basename script in
    let _ = Bos.OS.Cmd.run (copy_artifact artifact tmp') in
    let start = Unix.gettimeofday () in
    let _ = Bos.OS.Cmd.run (run_script script') in
    let finish = Unix.gettimeofday () in
    Sys.remove tmp;
    Sys.remove script;
    finish -. start

end


let check_equal x y = compare_check x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
    if List.mem ys c ~equal:check_equal then ac
    else c :: ac)

let arti_checks a = Artifact.checks a |> List.map ~f:fst

let update_results ?(out="results.html") artis =
  let doc = Template.render (Map.data artis) in
  Out_channel.with_file out
    ~f:(fun ch -> Out_channel.output_string ch doc)

let update_time arti checks time =
  List.fold checks ~init:arti
      ~f:(fun arti c -> Artifact.with_time arti c time)

let str_of_checks cs =
  List.fold cs ~init:""
    ~f:(fun s c -> sprintf "%s%s "
                  s (Sexp.to_string (sexp_of_check c)))

let pull artifacts =
  List.iter artifacts ~f:(fun a ->
      if not (Docker.image_exists a) then Docker.pull a)

let run artifacts recipes =
  pull artifacts;
  let artis = Bap_report_parse_incidents.parse_confirmations
              "confirmations" in
  let artis' = List.fold artis ~init:(Map.empty (module String))
                ~f:(fun m a -> Map.set m (Artifact.name a) a) in
  List.fold artifacts ~init:(Map.empty (module String))
    ~f:(fun artis name ->
      let size = Docker.size name in
      let arti = Artifact.create ?size name in
      let artis = Map.set artis name arti in
      List.fold ~init:artis recipes ~f:(fun artis r ->
          let arti = Map.find_exn artis name in
          let checks = arti_checks arti in
          let time = Docker.run_recipe arti r in
          let arti = Bap_report_parse_incidents.process arti "incidents" in
          let checks = check_diff (arti_checks arti) checks in
          let arti = update_time arti checks time in
          let arti = match Map.find artis' (Artifact.name arti) with
            | None -> arti
            | Some arti' ->
               List.fold ~init:arti checks
                 ~f:(fun arti c ->
                   match Artifact.find_check arti' c with
                   | [] -> arti
                   | rs ->
                      List.fold rs ~init:arti
                        ~f:(fun a (r,s) -> Artifact.update a c r s)) in
          let artis = Map.set artis (Artifact.name arti) arti in
          update_results artis;
          artis)) |> ignore

let () =
  let artifacts = ["test-only"; "test-only-2";] in
  let recipes = ["defective-symbol"; "av-rule-174"; "jpl-rule-14"] in
  run artifacts recipes
