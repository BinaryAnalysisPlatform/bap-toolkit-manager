open Core_kernel
open Cmdliner
open Bap_report.Std

module Config = Bap_report_config
module Cmd = Bap_report_cmd_terms

open Bap_report_scheduled

type mode =
  | From_incidents of string
  | From_stored    of string
  | Run_artifacts  of (string list * recipe list) list

type t = {
  mode       : mode;
  context    : Job.ctxt;
  confirms   : string option;
  output     : string;
  store      : string option;
  update     : bool;
} [@@deriving fields]

let find_recipe tool r =
  let name = Cmd.requested_name r in
  match Tool.find_recipe tool name with
  | None -> Or_error.errorf "can't find recipe %s\n" name
  | Some recipe ->
    let recipe =
      List.fold (Cmd.requested_pars r) ~init:recipe
        ~f:(fun recipe (p,v) ->
            Recipe.add_parameter recipe p v) in
    Ok recipe

let all_recipes tool = Tool.recipes tool

let create_recipes tool recipes =
  match List.find recipes ~f:(fun r -> Cmd.requested_name r = "all") with
  | Some _ -> Ok (all_recipes tool)
  | None ->
    Result.all @@ List.map recipes ~f:(find_recipe tool)

let create_recipes config tool recipes =
  Or_error.map (create_recipes tool recipes) ~f:(fun rs ->
      List.map rs ~f:(Config.provide_kinds config))

let read_config = function
  | None -> Config.empty
  | Some f -> Config.read f

let print_recipes_and_exit tool =
  let recipes = Tool.recipes tool in
  List.iter recipes ~f:(fun r ->
      printf "%-32s %s\n" (Recipe.name r) (Recipe.description r));
  exit 0

let print_bap_version_and_exit tool =
  match Tool.bap_version tool with
  | None ->
     eprintf "bap not found in %s\n" (Tool.to_string tool);
     exit 1
  | Some str -> printf "bap version: %s" str; exit 0

let print_and_exit tool recipes version =
  if recipes then print_recipes_and_exit tool;
  if version then print_bap_version_and_exit tool

let create tool print_recipes print_bap_version mode ctxt conf out store update =
  print_and_exit tool print_recipes print_bap_version;
  Fields.create mode ctxt conf out store update

let check_if_nothing_to_do xs =
  let notify_and_exit what =
    eprintf "the %s list is empty, nothing to do!\n" what;
    exit 1 in
  let names = List.map xs ~f:fst in
  let recipes = List.map xs ~f:snd in
  if List.for_all names ~f:List.is_empty then
    notify_and_exit "artifacts";
  if List.for_all recipes ~f:List.is_empty then
    notify_and_exit "recipes"

let make_run tool = function
  | Error er ->
    eprintf "%s\n" @@ Error.to_string_hum er;
    exit 1
  | Ok xs ->
     check_if_nothing_to_do xs;
     Run_artifacts xs

let infer_mode tool config of_schedule of_file of_incidents artifacts recipes =
  let (>>=) = Or_error.(>>=) in
  match of_schedule, of_file, of_incidents with
  | Some f,_,_ ->
    let acts = Bap_report_scheduled.of_file f in
    let rs =
      List.fold acts
        ~init:(Ok [])
        ~f:(fun acc s ->
            acc >>= fun acc ->
            create_recipes config tool s.recipes >>= fun rs ->
            Ok (([s.artifact], rs) :: acc)) in
    make_run tool rs
  | _,Some f,_ -> From_stored f
  | _,_,Some f -> From_incidents f
  | _ ->
    let rs =
      Ok (List.concat artifacts) >>= fun artis ->
      create_recipes config tool (List.concat recipes) >>= fun recipes ->
      Ok [artis,recipes] in
    make_run tool rs

let tool_of_string s =
  let tool = match s with
    | "host" -> Tool.host ()
    | s ->
       Or_error.(Image.of_string s >>= Tool.of_image) in
  match tool with
  | Ok t -> t
  | Error er ->
     eprintf "can't find or create tool %s: %s" s (Error.to_string_hum er);
     exit 1

let context tool limits verbose =
  let limit = List.fold limits
                ~init:Limit.empty ~f:(fun l (n,q) -> Limit.add l n q) in
  Job.context ~verbose ~limit tool

open Cmd
open Term

let options =
  let tool   = const tool_of_string $tool in
  let config = const read_config $config in
  let ctxt = const context $tool $limits $verbose in
  let mode = const infer_mode
                   $tool $config $schedule
                   $of_file $of_incidents
                   $artifacts $recipes in
  const create
        $tool $list_recipes $bap_version
        $mode $ctxt $confirms $report
        $store $update
