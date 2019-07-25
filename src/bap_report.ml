open Core_kernel
open Bap_report_types

module Template = Bap_report_template
module Docker = Bap_report_docker

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

let run artifacts recipes =
  let artis = Bap_report_parse_incidents.parse_confirmations
              "confirmations" in
  let artis' = List.fold artis ~init:(Map.empty (module String))
                ~f:(fun m a -> Map.set m (Artifact.name a) a) in
  List.fold artifacts ~init:(Map.empty (module String))
    ~f:(fun artis name ->
      match Docker.find_artifact name with
      | None -> artis
      | Some arti ->
         let artis = Map.set artis name arti in
         List.fold ~init:artis recipes ~f:(fun artis r ->
             let arti = Map.find_exn artis name in
             let checks = arti_checks arti in
             let doc_res = Docker.run_recipe arti r in
             let arti = Bap_report_parse_incidents.process arti "incidents" in
             let checks = check_diff (arti_checks arti) checks in
             let arti = update_time arti checks (Docker.time_taken doc_res) in
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
  let recipes = ["defective-symbol";] in
  run artifacts recipes
