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

type job_ctxt = {
  tool    : image;
  limit   : limit;
  verbose : bool;
}

type t = {
  mode       : mode;
  context    : job_ctxt;
  confirms   : string option;
  output     : string;
  store      : string option;
  update     : bool;
} [@@deriving fields]

let find_recipe tool r =
  let name = Cmd.requested_name r in
  match Recipe.find tool name with
  | None -> Or_error.errorf "can't find recipe %s\n" name
  | Some recipe ->
    let recipe =
      List.fold (Cmd.requested_pars r) ~init:recipe
        ~f:(fun recipe (p,v) ->
            Recipe.add_parameter recipe p v) in
    Ok recipe

let all_recipes tool = Recipe.list tool

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

let create mode ctxt conf out store update =
  Fields.create mode ctxt conf out store update

let make_run = function
  | Error er ->
    eprintf "%s\n" @@ Error.to_string_hum er;
    exit 1
  | Ok xs -> Run_artifacts xs

let infer_mode ctxt config of_schedule of_file of_incidents artifacts recipes =
  let (>>=) = Or_error.(>>=) in
  match of_schedule, of_file, of_incidents with
  | Some f,_,_ ->
    let acts = Bap_report_scheduled.of_file f in
    let rs =
      List.fold acts
        ~init:(Ok [])
        ~f:(fun acc s ->
            acc >>= fun acc ->
            create_recipes config ctxt.tool s.recipes >>= fun rs ->
            Ok (([s.artifact], rs) :: acc)) in
    make_run rs
  | _,Some f,_ -> From_stored f
  | _,_,Some f -> From_incidents f
  | _ ->
    let rs =
      Ok (List.concat artifacts) >>= fun artis ->
      create_recipes config ctxt.tool (List.concat recipes) >>= fun recipes ->
      Ok [artis,recipes] in
    make_run rs

let context tool limits verbose =
  match Docker.Image.of_string tool with
  | Error er ->
    eprintf "can't find tool %s: %s" tool (Error.to_string_hum er);
    exit 1
  | Ok tool ->
    let limit = List.fold limits
        ~init:Limit.empty ~f:(fun l (n,q) -> Limit.add l n q) in
    {tool; verbose; limit}

open Cmd

let options =
  let config = Term.(const read_config $config) in
  let ctxt = Term.(const context $tool $limits $verbose) in
  let mode = Term.(const infer_mode
                   $ctxt
                   $config
                   $schedule
                   $of_file
                   $of_incidents
                   $artifacts
                   $recipes) in
  Term.(const create
        $mode
        $ctxt
        $confirms
        $output
        $store
        $update)
