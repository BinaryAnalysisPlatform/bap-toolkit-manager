open Core_kernel
open Bap_report.Std

module Bap_artifact = struct
  let image = "binaryanalysisplatform/bap-artifacts"

  type kind =
    | Local
    | Image

  let run_recipe a kind r =
    match kind with
    | Local -> Recipe.run (Artifact.name a) r
    | Image ->  Recipe.run ~image ~tag:(Artifact.name a) "/artifact" r

  let can't_find tag reason =
    eprintf "can't find %s: %s\n" tag reason

  let artifact_exists tag =
    match Docker.get_image ~tag image with
    | Error er ->
       can't_find tag (Error.to_string_hum er);
       false
    | Ok () ->
       let cmd = sprintf "find / -type f -name /artifact" in
       match Docker.run ~image ~tag cmd with
       | None | Some "" ->
          can't_find tag "no such file in image";
          false
       | _ -> true

  let kind_of_name name =
    if Sys.file_exists name then Some Local
    else if artifact_exists name then Some Image
    else None

  let find name =
    match kind_of_name name with
    | None -> None
    | Some Local ->
       let size = size name in
       Some (Local, Artifact.create ?size name)
    | Some Image ->
       let size = size ~image ~tag:name "/artifact" in
       Some (Image, Artifact.create ?size name)

end

let check_equal x y = compare_check x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

let arti_checks a = Artifact.checks a

module Render = struct

  type t = (Bap_artifact.kind * artifact) String.Map.t * string

  let create file = Map.empty (module String), file

  let update (t, out) (kind,arti) =
    Map.set t (Artifact.name arti) (kind,arti), out

  let get (t,_) name = Map.find t name

  let run (t,out) =
    let artis = Map.data t |> List.map ~f:snd in
    let doc = Template.render artis in
    Out_channel.with_file out
      ~f:(fun ch -> Out_channel.output_string ch doc)

end

let update_time arti checks time =
  List.fold checks ~init:arti
    ~f:(fun arti c -> Artifact.with_time arti c time)

let find_confirmations path = []
  (* let incs = In_channel.with_file path ~f:Read.confirmations in
   * List.fold incs ~init:(Map.empty (module String))
   *   ~f:(fun m (i,s) -> Map.set m ~key:(Artifact.name a) ~data:a) *)


let confirm confirmations arti checks = arti
  (* match Map.find confirmations (Artifact.name arti) with
   * | None -> arti
   * | Some arti' ->
   *    List.fold ~init:arti checks
   *      ~f:(fun arti c ->
   *        match Artifact.find_result arti' c with
   *        | [] -> arti
   *        | rs ->
   *           List.fold rs ~init:arti
   *             ~f:(fun a (r,s) -> Artifact.update a c r s)) *)

let run_artifact ?confirmations arti kind recipe =
  let checks = arti_checks arti in
  let recipe = Bap_artifact.run_recipe arti kind recipe in
  let time = Recipe.time_taken recipe in
  let incs = "incidents" in
  if Sys.file_exists incs then
    let incs = In_channel.with_file incs ~f:Read.incidents in
    let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
    let checks = check_diff (arti_checks arti) checks in
    let arti = update_time arti checks time  in
    match confirmations with
    | None -> arti
    | Some confirmations -> confirm confirmations arti checks
  else arti

let need_all names =
  let names = List.map ~f:String.lowercase names in
  List.mem names "all" ~equal:String.equal

let recipes_of_names names =
  let recipes = Recipe.list () in
  if need_all names then recipes
  else
  List.filter recipes
    ~f:(fun r -> List.mem names (Recipe.name r) ~equal:String.equal)

let run render name ?confirmations recipes =
  let recipes = recipes_of_names recipes in
  match Render.get render name with
  | None -> render
  | Some (kind,arti) ->
     List.fold ~init:(render,arti) recipes ~f:(fun (render,arti) reci ->
         let arti = run_artifact ?confirmations arti kind reci in
         let render = Render.update render (kind,arti) in
         Render.run render;
         render,arti) |> fst

(*
TODO: check conirmations!!
TODO: remove incidents file on exit ??
TODO: find a way to limit time?
 *)

let run_artifacts out artis recipes =
  let render =
    List.fold artis
      ~init:(Render.create out) ~f:(fun r name ->
          match Bap_artifact.find name with
          | None ->
             printf "didn't find artifact %s\n" name;
             r
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
          match Bap_artifact.find name with
          | None -> r
          | Some a -> Render.update r a) in
  ignore @@
    List.fold acts
      ~init:render
      ~f:(fun render (name,recipes) -> run render name recipes)

let check_toolkit () =
  let tool = "binaryanalysisplatform/bap-toolkit" in
  match Docker.get_image tool with
  | Ok () -> ()
  | Error _ ->
     eprintf "can't detect/pull bap-toolkit, exiting ... ";
     exit 1

module O = struct

  type t = {
    config    : string option;
    artifacts : string list;
    recipes   : string list;
    confirms  : string option;
    output    : string;
  } [@@deriving fields]


  let create a b c d e = Fields.create a b c d e

end

open Cmdliner

let doc = "Bap report"

let man = [
    `S "SYNOPSIS";
    `Pre "
      $(mname) --artifacts=... --recipes=...
      $(mname) --artifacts=... --recipes=... --confirmations=...
      $(mname) --config=...
      $(mname) --list-recipes
      $(mname) --list-artifacts";

    `S "Description";
    `P "A frontend to the whole bap and docker infrastructures,
        that hides all the complexity under the hood: no bap
        installation required, no manual pulling of docker
        images needed.";

    `P  "It allows easily to run
        the various of checks against the various of artifacts
        and get a frendly HTML report with all the incidents found.";

  ]

let info = Term.info ~man ~doc "bap-report"

let config =
  Arg.(value & opt (some string) None & info ~doc:"" ["from-config"])

let strings = Arg.(list string)

let artifacts =
  let doc = "A comma-separated list of artifacts to check.
             Every artifact is either a file in the system
             or a TAG from binaryanalysisplatform/bap-artifacts
             docker image" in
  Arg.(value & opt strings [] & info ["artifacts"] ~doc)

let recipes =
  let doc = "list of recipes to run. A special key $(i,all)
             can be used to run all the recipes" in
  Arg.(value & opt strings [] & info ["recipes"] ~doc)

let confirms =
  let doc = "file with confirmations" in
  Arg.(value & opt (some non_dir_file) None & info ["confirmations"] ~doc)

let output =
  let doc = "file with results" in
  Arg.(value & opt string "results.html" & info ["output"] ~doc)

let list_recipes =
  let doc = "prints the list of available recipes and exits" in
  Arg.(value & flag & info ["list-recipes"] ~doc)

let list_artifacts =
  let doc = "prints list of available artifacts and exits" in
  Arg.(value & flag & info ["list-artifacts"] ~doc)


let is_specified opt ~default =
  Cmdliner.Term.eval_peek_opts opt |>
  fst |> Option.value ~default

let print_recipes_and_exit () =
  let recipes = Recipe.list ()  in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_artifacts_and_exit () =
  let images = Docker.available_tags Bap_artifact.image in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let main o print_recipes print_artifacts =
  let open O in
  check_toolkit ();
  if print_recipes then print_recipes_and_exit ();
  if print_artifacts then
    print_artifacts_and_exit ();
  match o.config with
  | None -> run_artifacts o.output o.artifacts o.recipes
  | Some cnf -> run_config o.output cnf

let o =
  Term.(const O.create
        $config
        $artifacts
        $recipes
        $confirms
        $output)

let _ = Term.eval (Term.(const main $o $list_recipes $list_artifacts), info)
