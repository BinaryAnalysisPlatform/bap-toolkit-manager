open Core_kernel
open Bap_report.Std
open Bap_report_options

module Scheduled = Bap_report_scheduled

type artifact_kind =
  | Local
  | Image

type t = {
  ctxt  : job_ctxt;
  artis : (artifact_kind * artifact) String.Map.t;
  confirmed: confirmation Incident.Id.Map.t String.Map.t;
  output  : string;
}

let create ctxt output confirmed =
  { ctxt; artis = Map.empty (module String); output; confirmed }

let update t (kind,arti) =
  {t with artis =
            Map.set t.artis (Artifact.name arti) (kind,arti) }

let get t name = Map.find t.artis name
let artifacts t = Map.data t.artis |> List.map ~f:snd

module Bap_artifact = struct

  let image = Docker.Image.of_string_exn "binaryanalysisplatform/bap-artifacts"

  let with_tag tag = Docker.Image.with_tag image tag

  let run ctxt =
    Job.run ~verbose:ctxt.verbose ~tool:ctxt.tool ~limit:ctxt.limit

  let run_recipe ctxt arti kind r  =
    let run = run ctxt r in
    match kind with
    | Local -> run (Artifact.name arti)
    | Image -> run ~image:(with_tag (Artifact.name arti)) "/artifact"

  let can't_find tag reason = eprintf "can't find %s: %s\n" tag reason

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

let render t =
  let doc = Template.render (artifacts t) in
  Out_channel.with_file t.output
    ~f:(fun ch -> Out_channel.output_string ch doc)

let check_equal x y = compare_incident_kind x y = 0

let check_diff xs ys =
  List.fold xs ~init:[] ~f:(fun ac c ->
      if List.mem ys c ~equal:check_equal then ac
      else c :: ac)

let update_time arti checks time =
  List.fold checks ~init:arti
    ~f:(fun arti c -> Artifact.with_time arti c time)

let map_of_alist ~init xs =
  List.fold ~init xs ~f:(fun m conf -> Map.set m (Confirmation.id conf) conf)

let read_confirmations = function
  | None -> Map.empty (module String)
  | Some path ->
    let confs = In_channel.with_file path ~f:Read.confirmations in
    List.fold confs ~init:(Map.empty (module String))
      ~f:(fun m (name,confs) ->
          Map.update m name ~f:(function
              | None -> map_of_alist ~init:Incident.Id.Map.empty confs
              | Some confs' -> map_of_alist ~init:confs' confs))

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

let print_errors job =
  List.iter (Job.errors job) ~f:(eprintf "%s\n")

let startup_time () =
  let open Unix in
  let t = gettimeofday () |> localtime in
  sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec

let missed_kinds recipe incidents =
  let provides = Recipe.kinds recipe |> Set.of_list (module Incident.Kind) in
  let happened = List.fold incidents ~init:(Set.empty (module Incident.Kind))
      ~f:(fun kinds inc -> Set.add kinds (Incident.kind inc)) in
  Set.diff provides happened |> Set.to_list

let run_artifact t arti kind recipe =
  printf "started %s %s at %s\n%!"
    (Artifact.name arti)
    (Recipe.to_string recipe)
    (startup_time ());
  let checks = Artifact.checks arti in
  let job = Bap_artifact.run_recipe t.ctxt arti kind recipe in
  print_errors job;
  let incs = Job.incidents job in
  let missed = missed_kinds recipe incs in
  let arti = List.fold missed ~init:arti
      ~f:(fun arti kind ->
          let a = Artifact.no_incidents arti kind in
          Artifact.with_time a kind (Job.time job)) in
  let arti = List.fold incs ~init:arti ~f:(fun a i -> Artifact.update a i Undecided) in
  let diff = check_diff (Artifact.checks arti) checks in
  let arti = update_time arti diff (Job.time job)  in
  confirm t.confirmed arti diff

let run t name recipes =
  match get t name with
  | None -> t
  | Some (kind,arti) ->
    List.fold ~init:(t,arti) recipes ~f:(fun (t,arti) recipe ->
        let arti = run_artifact t arti kind recipe in
        let t = update t (kind,arti) in
        render t;
        t,arti) |> fst

let run_artifacts t tasks  =
  List.fold tasks ~init:t
    ~f:(fun t (names, recipes) ->
        List.fold ~init:t names
          ~f:(fun t name -> run t name recipes))

let create_artifacts t artis =
  List.fold artis
    ~init:t ~f:(fun r name ->
        match Bap_artifact.find name with
        | None ->
          eprintf "didn't find artifact %s, skipping ... \n" name;
          r
        | Some a -> update r a)

let of_incidents_file t filename =
  let name = Filename.remove_extension filename in
  let incidents = In_channel.with_file filename ~f:Read.incidents in
  let artifact = Artifact.create name in
  let artifact = List.fold incidents ~init:artifact
      ~f:(fun a i -> Artifact.update a i Undecided) in
  let artifact = confirm t.confirmed artifact (Artifact.checks artifact) in
  let t = update t (Local, artifact) in
  render t

let print_recipes_and_exit ctxt =
  let recipes = Recipe.list ctxt.tool  in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_artifacts_and_exit () =
  let images = Docker.Image.tags Bap_artifact.image in
  List.iter images ~f:(fun tag -> printf "%s\n" tag);
  exit 0

let print_bap_version ctxt =
  match Docker.run ctxt.tool "--version" with
  | None -> ()
  | Some str -> printf "bap version: %s" str

let main o print_recipes print_artifacts =
  let save artis = match o.store with
    | None -> ()
    | Some file -> Bap_report_io.dump file artis in
  if print_recipes   then print_recipes_and_exit o.context;
  if print_artifacts then print_artifacts_and_exit ();
  let confirmed = read_confirmations o.confirms in
  let t = create o.context o.output confirmed in
  let t = match o.store, o.update with
    | Some file, true ->
      let artis = Bap_report_io.read file in
      List.fold artis ~init:t ~f:(fun t a -> update t (Local,a))
    | _ -> t in
  match o.mode with
  | From_incidents incs -> of_incidents_file t incs
  | From_stored db ->
    let artis = Bap_report_io.read db in
    let t = List.fold artis ~init:t ~f:(fun t a -> update t (Local,a)) in
    render t
  | Run_artifacts tasks ->
    print_bap_version o.context;
    let artis =
      List.fold tasks ~init:[]
        ~f:(fun acc (artis,_) -> acc @ artis) in
    let t = create_artifacts t artis in
    let t = run_artifacts t tasks in
    save (artifacts t)

let _ =
  let open Cmdliner in
  let open Bap_report_cmd_terms in
  Term.eval (Term.(const main $options $list_recipes $list_artifacts), info)

(* TODO: install view file somewhere
   TODO: should be val Recipe.provides : t -> incident_kind list *)
