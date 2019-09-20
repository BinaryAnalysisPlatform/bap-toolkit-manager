open Core_kernel
open Bap_report.Std
open Bap_report_options

module Scheduled = Bap_report_scheduled


module Bap_artifact = struct

  let image = Docker.Image.of_string_exn "binaryanalysisplatform/bap-artifacts"

  let with_tag tag = Docker.Image.with_tag image tag

  type kind =
    | Local
    | Image

  let run_recipe tool arti kind limit r =
    match kind with
    | Local -> Job.run r ~tool ~limit (Artifact.name arti)
    | Image ->
      let image = with_tag (Artifact.name arti) in
      Job.run r ~tool ~image ~limit "/artifact"

  let can't_find tag reason =
    eprintf "can't find %s: %s\n" tag reason

  let artifact_exists tag =
    let image = with_tag tag in
    match Docker.Image.get image with
    | Error er ->
      can't_find tag (Error.to_string_hum er);
      false
    | Ok () ->
      let cmd = sprintf "find / -type f -name /artifact" in
      match Docker.run image cmd with
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
      let image = with_tag name in
      let size = Size.get ~image "/artifact" in
      Some (Image, Artifact.create ?size name)

end

let check_equal x y = compare_incident_kind x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

module Runner = struct

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

let print_bap_version tool =
  match Docker.run tool "--version" with
  | None -> ()
  | Some str -> printf "bap version: %s" str

let print_errors job =
  List.iter (Job.errors job)
    ~f:(fun errs ->
      List.iter errs ~f:(eprintf "%s\n");
      eprintf "\n")

let run_artifact tool confirmed arti kind recipe limit =
  printf "running %s %s\n%!" (Artifact.name arti) (Recipe.to_string recipe);
  let checks = Artifact.checks arti in
  let job = Bap_artifact.run_recipe tool arti kind limit recipe in
  print_errors job;
  match Job.incidents job with
  | [] -> arti
  | incs ->
    let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
    let checks = check_diff (Artifact.checks arti) checks in
    let arti = update_time arti checks (Job.time job)  in
    confirm confirmed arti checks

let need_all names =
  let f {Scheduled.name} = String.lowercase name = "all" in
  List.exists names ~f

let recipes_of_names tool names =
  let recipes = Recipe.list tool in
  let find name =
    List.find recipes ~f:(fun r -> String.equal (Recipe.name r) name) in
  if need_all names then recipes
  else
    List.filter_map names
      ~f:(fun {Scheduled.name;pars} ->
          (match find name with
           | None -> None
           | Some r ->
             List.fold pars ~init:r ~f:(fun r (name,value) ->
                 Recipe.add_parameter r ~name ~value) |>
             Option.some))

let run tool runner confirmed name recipes limit =
  let recipes = recipes_of_names tool recipes in
  match Runner.get runner name with
  | None -> runner
  | Some (kind,arti) ->
    List.fold ~init:(runner,arti) recipes ~f:(fun (runner,arti) reci ->
        let arti = run_artifact tool confirmed arti kind reci limit in
        let runner = Runner.update runner (kind,arti) in
        Runner.run runner;
        runner,arti) |> fst

let run_artifacts tool runner confirmed artis recipes limit =
  let runner =
    List.fold artis
      ~init:runner ~f:(fun r name ->
          match Bap_artifact.find name with
          | None ->
            eprintf "didn't find artifact %s, skipping ... \n" name;
            r
          | Some a -> Runner.update r a) in
  let runner =
    List.fold artis
      ~init:runner ~f:(fun runner name ->
          run tool runner confirmed name recipes limit) in
  Runner.artifacts runner

let run_schedule tool runner confirmed path limit =
  let acts = Scheduled.of_file path  in
  let runner =
    List.fold acts
      ~init:runner ~f:(fun r {Scheduled.artifact;} ->
          match Bap_artifact.find artifact with
          | None -> r
          | Some a -> Runner.update r a) in
  let runner =
    List.fold acts
      ~init:runner
      ~f:(fun runner s ->
          run tool runner confirmed s.artifact s.recipes limit) in
  Runner.artifacts runner

let check_toolkit tool =
  let (>>=) = Or_error.(>>=) in
  match Docker.Image.(of_string tool >>= fun tool -> get tool >>= fun () -> Ok tool) with
  | Ok tool -> tool
  | Error er ->
    eprintf "can't detect/pull toolkit: %s, exiting ... \n"
      (Error.to_string_hum er);
    exit 1

let of_incidents_file confirmations runner filename =
  let name = Filename.remove_extension filename in
  let incidents = In_channel.with_file filename ~f:Read.incidents in
  let artifact = Artifact.create name in
  let artifact = List.fold incidents ~init:artifact
      ~f:(fun a i -> Artifact.update a i Undecided) in
  let artifact = confirm confirmations artifact (Artifact.checks artifact) in
  let x = Runner.update runner (Local, artifact) in
  Runner.run x

module O = struct

  type t = {
    schedule   : string option;
    artifacts  : string list;
    recipes    : Scheduled.requested_recipe list;
    confirms   : string option;
    output     : string;
    of_incs    : string option;
    tool       : string;
    view       : string option;
    store      : string option;
    update     : bool;
    of_db      : string option;
    limits     : (int * Job.Limit.quantity) list;
  } [@@deriving fields]

  let create a b recipes d e f g h i j k l =
    Fields.create a b (List.concat recipes) d e f g h i j k l

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
  let images = Docker.Image.tags Bap_artifact.image in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let main o print_recipes print_artifacts =
  let open O in
  let save artis =
    match o.store with
    | None -> ()
    | Some file -> Bap_report_io.dump file artis in
  let tool = check_toolkit o.tool in
  if print_recipes   then print_recipes_and_exit tool;
  if print_artifacts then print_artifacts_and_exit ();
  let confirmed = match o.confirms with
    | None -> Map.empty (module String)
    | Some path -> read_confirmations path in
  let view = match o.view with
    | None -> View.create ()
    | Some f -> View.of_file f in
  let runner = Runner.create view o.output in
  let runner = match o.store, o.update with
    | Some file, true ->
      let artis = Bap_report_io.read file in
      List.fold artis ~init:runner ~f:(fun r a -> Runner.update r (Local,a))
    | _ -> runner in
  let limit = List.fold o.limits
                ~init:Job.Limit.empty ~f:(fun l (n,q) -> Job.Limit.add l n q) in
  match o.schedule, o.of_incs, o.of_db with
  | Some sch, _, _ ->
    print_bap_version tool;
    let artis = run_schedule tool runner confirmed sch limit in
    save artis
  | _, Some file,_ -> of_incidents_file confirmed runner file
  | _,_, Some db ->
    let artis = Bap_report_io.read db in
    let runner = List.fold artis ~init:runner ~f:(fun r a ->
        Runner.update r (Local,a)) in
    Runner.run runner
  | _ ->
    print_bap_version tool;
    run_artifacts tool runner confirmed o.artifacts o.recipes limit |>
    save

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
        $of_file
        $limits)

let _ = Term.eval (Term.(const main $o $list_recipes $list_artifacts), info)

(*
TODO: document everything
TODO: install view file somewhere
*)
