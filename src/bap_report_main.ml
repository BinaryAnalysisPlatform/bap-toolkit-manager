open Core_kernel
open Bap_report.Std
open Bap_report_types

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
  let confirm arti arti' checks =
    List.fold ~init:arti checks
      ~f:(fun arti c ->
        match Artifact.find_check arti' c with
        | [] -> arti
        | rs ->
           List.fold rs ~init:arti
             ~f:(fun a (r,s) -> Artifact.update a c r s)) in
  let artis = Parse.parse_confirmations
              "confirmations" in
  let artis' = List.fold artis ~init:(Map.empty (module String))
                ~f:(fun m a -> Map.set m ~key:(Artifact.name a) ~data:a) in
  List.fold artifacts ~init:(Map.empty (module String))
    ~f:(fun artis name ->
      match Docker.find_artifact name with
      | None -> artis
      | Some arti ->
         let artis = Map.set artis ~key:name ~data:arti in
         List.fold ~init:artis recipes ~f:(fun artis r ->
             let arti = Map.find_exn artis name in
             let checks = arti_checks arti in
             let doc_res = Docker.run_recipe arti r in
             let arti = Parse.process arti "incidents" in
             let checks = check_diff (arti_checks arti) checks in
             let arti = update_time arti checks (Docker.time_taken doc_res) in
             let arti = match Map.find artis' (Artifact.name arti) with
               | None -> arti
               | Some arti' -> confirm arti arti' checks in
             let artis = Map.set artis ~key:(Artifact.name arti) ~data:arti in
             update_results artis;
          artis)) |> ignore

(* todo: maybe add option to run all recipes?
   todo: add configuration to run a specific rules for specific artifacts?
   todo: add option to run files as well
*)
let () =
  let artifacts = ["juliet-cwe-252"; "juliet-cwe-476"; "httpd-2.4.18";
                   "samba-4.7.6"; "openssl-1.1.0"] in
  let recipes = ["defective-symbol"; "av-rule-174"; "jpl-rule-14"; "primus-checks"] in
  run artifacts recipes
