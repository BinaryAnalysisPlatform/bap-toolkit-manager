open Core_kernel
open Bap_report.Std
open Bap_report_types


let check_equal x y = compare_check x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

let arti_checks a = Artifact.checks a |> List.map ~f:fst

module Render = struct

  type t = string * artifact String.Map.t

  let create file = file, Map.empty (module String)

  let update (out, t) arti =
    out, Map.set t (Artifact.name arti) arti

  let get (_,t) name = Map.find t name

  let run (out,t) =
    let doc = Template.render (Map.data t) in
    Out_channel.with_file out
      ~f:(fun ch -> Out_channel.output_string ch doc)

end

let update_time arti checks time =
  List.fold checks ~init:arti
    ~f:(fun arti c -> Artifact.with_time arti c time)

let find_artifact = Docker.find_artifact

let find_artifacts names =
  List.filter_map names ~f:Docker.find_artifact

let find_confirmations path =
  let artis = Parse.parse_confirmations path in
  List.fold artis ~init:(Map.empty (module String))
    ~f:(fun m a -> Map.set m ~key:(Artifact.name a) ~data:a)

let confirm confirmations  arti checks =
  match Map.find confirmations (Artifact.name arti) with
  | None -> arti
  | Some arti' ->
     List.fold ~init:arti checks
       ~f:(fun arti c ->
         match Artifact.find_check arti' c with
         | [] -> arti
         | rs ->
            List.fold rs ~init:arti
              ~f:(fun a (r,s) -> Artifact.update a c r s))

let run_artifact ?confirmations arti recipe =
  let checks = arti_checks arti in
  match Docker.run_recipe arti recipe with
  | None -> arti
  | Some result ->
    let arti = Parse.process arti "incidents" in
    let checks = check_diff (arti_checks arti) checks in
    let arti = update_time arti checks (Docker.time_taken result) in
    match confirmations with
    | None -> arti
    | Some confirmations -> confirm confirmations arti checks

let run render name ?confirmations recipes =
  match Render.get render name with
  | None -> render
  | Some arti ->
     List.fold ~init:(render,arti) recipes ~f:(fun (render,arti) reci ->
         let arti = run_artifact ?confirmations arti reci in
         let render = Render.update render arti in
         Render.run render;
         render,arti) |> fst

(*
TODO: add check that docker itself exists
TODO: make sure that bap-toolkit image is also present in docker
TODO: remove incidents file on exit
TODO: maybe add option to run all recipes or at least list all the recipes?
TODO: find a way to limit time?
TODO: edit config option - make it optional
 *)

let run_artifacts out artis recipes =
  let render =
    List.fold artis
      ~init:(Render.create out) ~f:(fun r name ->
          match Docker.find_artifact name with
          | None -> r
          | Some a -> Render.update r a) in
  ignore @@
    List.fold artis
      ~init:render ~f:(fun render name -> run render name recipes)

let parse_path = function
  | [Sexp.Atom x] -> Some x
  | _ -> None

let parse_actions = function
  | [Sexp.Atom target; Sexp.List recipes] ->
    let recipes = List.map recipes ~f:Sexp.to_string in
    Some (target,recipes)
  | _ -> None

let read_config acc path =
  let rec read acc = function
    | [] -> acc
    | Sexp.List data :: xs ->
      let acc = match parse_actions data with
        | None -> acc
        | Some d -> d :: acc in
      read acc xs
    | _ :: xs -> read acc xs in
  let sexps =
    In_channel.with_file path ~f:Sexp.input_sexps in
  read [] sexps

let run_config out path =
  let acts = read_config [] path  in
  let render =
    List.fold acts
      ~init:(Render.create out) ~f:(fun r (name,recipes) ->
          match Docker.find_artifact name with
          | None -> r
          | Some a -> Render.update r a) in
  ignore @@
    List.fold acts
      ~init:render ~f:(fun render (name,recipes) ->
          run render name recipes)

let check_toolkit () =
  if not (Docker.toolkit_exists ()) then
    failwith "can't detect/pull bap-toolkit, exiting ... "


open Cmdliner

let info = Term.info ""

let config =
  Arg.(value & opt string "" & info ~doc:"" ["from-config"])

let strings = Arg.(list string)

let artifacts =
  let doc = "list of artifacts" in
  Arg.(value & opt strings [] & info ["artifacts"] ~doc)

let recipes =
  let doc = "list of recipes" in
  Arg.(value & opt strings [] & info ["recipes"] ~doc)

let confirmatins =
  let doc = "file with confirmations" in
  Arg.(value & opt string "" & info ["confirmations"] ~doc)

let output =
  let doc = "file with results" in
  Arg.(value & opt string "results.html" & info ["output"] ~doc)

let list_recipes =
  let doc = "prints list of recieps and exits" in
  Arg.(value & flag & info ["list-recipes"] ~doc)

let dump =
  let doc = "dumps all the results in the sexp format" in
  Arg.(value & flag & info ["dump"] ~doc)

let main config artis recipes out =
  check_toolkit ();
  match config with
  | "" -> run_artifacts out artis recipes
  | cnf -> run_config out cnf

let c = Term.eval (Term.(const main $config $artifacts $recipes $output),info)
