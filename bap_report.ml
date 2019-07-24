open Core_kernel
open Bap.Std

open Bap_report_types

module Template = Bap_report_template
module Artifact = Template.Artifact

let (/) = Filename.concat

let file_exists file =
  try
    FileUtil.(test Exists file)
  with _ -> false

let parse_incidents arti file =
  let open Bap_report_parse_incidents in
  let data = run file in
  List.fold data ~init:arti ~f:(fun a (check,stat,data) ->
      match data with
      | [] -> a
      | data -> Artifact.add_check a check stat data)

let () =
  let files = FileUtil.ls "." in
  let tests, artis =
    List.fold files ~init:([],[]) ~f:(fun (tests,artis) dir ->
        let r = dir / "incidents" in
        if file_exists r then
          let name = Filename.basename dir in
          if name <> "juliet-cwe-252" then tests,artis
          else
          let size = "no matter" in
          let arti = Artifact.create ~name ~size in
          let arti = parse_incidents arti r in
          if String.is_substring name ~substring:"juliet" then
            arti :: tests, artis
          else tests, arti::artis
        else tests, artis) in
  let all = List.rev tests @ List.rev artis in
  let doc = Template.render all in
  Out_channel.with_file "results.html"
    ~f:(fun ch -> Out_channel.output_string ch doc)
