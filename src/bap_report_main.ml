open Core_kernel
open Bap_report.Std
open Bap_report_options

module Bap_artifact = struct
  let image = "binaryanalysisplatform/bap-artifacts"

  type kind =
    | Local
    | Image

  let run_recipe tool a kind r =
    match kind with
    | Local -> Recipe.run ~tool (Artifact.name a) r
    | Image ->  Recipe.run ~tool ~image ~tag:(Artifact.name a) "/artifact" r

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
      let size = Size.get name in
      Some (Local, Artifact.create ?size name)
    | Some Image ->
      let size = Size.get ~image ~tag:name "/artifact" in
      Some (Image, Artifact.create ?size name)

end

let check_equal x y = compare_incident_kind x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

module Render = struct

  type t = {
      view : View.t;
      arts : (Bap_artifact.kind * artifact) String.Map.t;
      outp : string;
    }

  let create view output = {
      view;
      arts = Map.empty (module String);
      outp = output;
    }

  let update t (kind,arti) =
    {t with arts =
      Map.set t.arts (Artifact.name arti) (kind,arti) }

  let get t name = Map.find t.arts name

  let run t =
    let artis = Map.data t.arts |> List.map ~f:snd in
    let doc = Template.render t.view artis in
    Out_channel.with_file t.outp
      ~f:(fun ch -> Out_channel.output_string ch doc)


  let artifacts t = Map.data t.arts |> List.map ~f:snd
end

let update_time arti checks time =
  List.fold checks ~init:arti
    ~f:(fun arti c -> Artifact.with_time arti c time)

let map_of_alist ~init xs =
  List.fold ~init xs ~f:(fun m conf -> Map.set m (Confirmation.id conf) conf)

let read_confirmations path =
  let confs = In_channel.with_file path ~f:Read.confirmations in
  List.fold confs ~init:(Map.empty (module String))
    ~f:(fun m (name,confs) ->
        Map.update m name ~f:(function
            | None -> map_of_alist ~init:Incident.Id.Map.empty confs
            | Some confs' -> map_of_alist ~init:confs' confs))

let check_mem checks c =
  List.mem checks c ~equal:(fun c c' -> Incident.Kind.compare c c' = 0)

let confirm confirmations arti kinds =
  match Map.find confirmations (Artifact.name arti) with
  | None -> arti
  | Some confirmed ->
     let checks = Artifact.checks arti in
     Map.fold confirmed ~init:arti ~f:(fun ~key:id ~data:conf arti ->
         let k = Confirmation.incident_kind conf in
         if not (List.mem checks k ~equal:Incident.Kind.equal) then arti
         else
         match Artifact.find arti id with
         | Some (inc,st) ->
            let status = Confirmation.validate conf (Some st) in
            Artifact.update arti inc status
         | None ->
            match Confirmation.validate conf None with
            | False_neg as status ->
               let inc = Incident.create
                           (Confirmation.locations conf)
                           (Confirmation.incident_kind conf)  in
               Artifact.update arti inc  status
            | _ -> arti)


let run_artifact tool confirmed arti kind recipe =
  printf "running %s %s\n%!" (Artifact.name arti) (Recipe.name recipe);
  let checks = Artifact.checks arti in
  let recipe = Bap_artifact.run_recipe tool arti kind recipe in
  let time = Recipe.time_taken recipe in
  let incs = "incidents" in
  if Sys.file_exists incs then
    let incs = In_channel.with_file incs ~f:Read.incidents in
    let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
    let checks = check_diff (Artifact.checks arti) checks in
    let arti = update_time arti checks time  in
    confirm confirmed arti checks
  else arti

let need_all names =
  let names = List.map ~f:String.lowercase names in
  List.mem names "all" ~equal:String.equal

let recipes_of_names tool names =
  let recipes = Recipe.list tool in
  if need_all names then recipes
  else
    List.filter recipes
      ~f:(fun r -> List.mem names (Recipe.name r) ~equal:String.equal)

let run tool render confirmed name recipes =
  let recipes = recipes_of_names tool recipes in
  match Render.get render name with
  | None -> render
  | Some (kind,arti) ->
    List.fold ~init:(render,arti) recipes ~f:(fun (render,arti) reci ->
        let arti = run_artifact tool confirmed arti kind reci in
        let render = Render.update render (kind,arti) in
        Render.run render;
        render,arti) |> fst

let default_view = View.create ()

let run_artifacts tool render confirmed artis recipes =
  let render =
    List.fold artis
      ~init:render ~f:(fun r name ->
          match Bap_artifact.find name with
          | None ->
            eprintf "didn't find artifact %s, skipping ... \n" name;
            r
          | Some a -> Render.update r a) in
  let render =
    List.fold artis
      ~init:render ~f:(fun render name ->
        run tool render confirmed name recipes) in
  Render.artifacts render


let parse_path = function
  | [Sexp.Atom x] -> Some x
  | _ -> None

let parse_actions = function
  | [Sexp.Atom target; Sexp.List recipes] ->
    let recipes = List.map recipes ~f:Sexp.to_string in
    Some (target,recipes)
  | [Sexp.Atom target; Sexp.Atom recipe] ->
    Some (target,[recipe])
  | _ -> None

let read_schedule acc path =
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
  read [] sexps |> List.rev

let run_schedule tool render confirmed path =
  let acts = read_schedule [] path  in
  let render =
    List.fold acts
      ~init:render ~f:(fun r (name,recipes) ->
          match Bap_artifact.find name with
          | None -> r
          | Some a -> Render.update r a) in
  let render =
  List.fold acts
    ~init:render
    ~f:(fun render (name,recipes) ->
      run tool render confirmed name recipes) in
  Render.artifacts render

let check_toolkit tool =
  let tag = Tool.tag tool in
  let image = Tool.name tool in
  match Docker.get_image ?tag image with
  | Ok () -> ()
  | Error _ ->
     eprintf "can't detect/pull bap-toolkit, exiting ... ";
     exit 1

let of_incidents_file render filename =
  let incidents = In_channel.with_file filename ~f:Read.incidents in
  let artifact = Artifact.create filename in
  let artifact = List.fold incidents ~init:artifact ~f:(fun a i ->
      Artifact.update a i Undecided) in
  let x = Render.update render (Local, artifact) in
  Render.run x

module O = struct

  type t = {
    schedule  : string option;
    artifacts : string list;
    recipes   : string list;
    confirms  : string option;
    output    : string;
    of_incs   : string option;
    tool      : string;
    view      : string option;
    store     : string option;
    update    : bool;
    of_db     : string option;

  } [@@deriving fields]

  let create a b c d e f g h i j k = Fields.create a b c d e f g h i j k

end

let is_specified opt ~default =
  Cmdliner.Term.eval_peek_opts opt |>
  fst |> Option.value ~default

let print_recipes_and_exit tool =
  let recipes = Recipe.list tool  in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_artifacts_and_exit () =
  let images = Docker.available_tags Bap_artifact.image in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let main o print_recipes print_artifacts =
  let open O in
  let save artis =
    match o.store with
    | None -> ()
    | Some file -> Bap_report_dump.dump file artis in
  let tool = match Tool.of_string o.tool with
       | Ok tool -> tool
       | Error er ->
          eprintf "%s\n" @@ Error.to_string_hum er;
          exit 1 in
  check_toolkit tool;
  if print_recipes   then print_recipes_and_exit tool;
  if print_artifacts then print_artifacts_and_exit ();
  let confirmed = match o.confirms with
    | None -> Map.empty (module String)
    | Some path -> read_confirmations path in
  let view = match o.view with
    | None -> default_view
    | Some f -> View.of_file f in
  let render = Render.create view o.output in
  let render = match o.store, o.update with
    | Some file, true ->
       let artis = Bap_report_dump.read file in
       List.fold artis ~init:render ~f:(fun r a -> Render.update r (Local,a))
    | _ -> render in
  match o.schedule, o.of_incs, o.of_db with
  | Some sch, _, _ ->
     let artis = run_schedule tool render confirmed sch in
     save artis
  | _, Some file,_ -> of_incidents_file render file
  | _,_, Some db ->
     let artis = Bap_report_dump.read db in
     let render = List.fold artis ~init:render ~f:(fun r a ->
                      Render.update r (Local,a)) in
     Render.run render
  | _ ->
     let artis = run_artifacts tool render confirmed
                   o.artifacts o.recipes in
     save artis


open Cmdliner

let o =
  Term.(const O.create
        $schedule
        $artifacts
        $recipes
        $confirms
        $output
        $of_incidents
        $tool
        $view
        $store
        $update
        $of_db)

let _ = Term.eval (Term.(const main $o $list_recipes $list_artifacts), info)

(*
TODO: document everything
TODO: install view file somewhere ?
TODO: there is a bug when in infering a size of an artifact
TODO: remove incidents file on exit
TODO: find a way to limit time?
*)
