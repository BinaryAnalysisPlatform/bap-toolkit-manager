open Core_kernel
open Cmdliner
open Bap_report.Std
open Bap_report_scheduled


module Recipes = struct
  type t = requested_recipe list

  let printer fmt recipes =
    List.iter recipes ~f:(fun {name;pars} ->
        let args = List.map pars ~f:(fun (a,b) -> sprintf "%s=%s" a b) in
        Format.fprintf fmt "%s with %s\n" name
          (String.concat ~sep:"," args))

  let with_no_pars xs = List.map xs ~f:(fun x -> {name=x; pars=[]})

  let parse_par a =
    match String.split a ~on:'=' with
    | [arg;value] -> arg,value
    | _ ->
      eprintf "can't parse argument %s, should be in the form arg=value\n" a;
      exit 1

  let parser : t Arg.parser = fun str ->
    match String.split ~on:':' str with
    | [names]  -> `Ok (with_no_pars (String.split ~on:',' names))
    | [name;pars] ->
      let pars = String.split pars ~on:',' in
      let pars = List.map pars ~f:parse_par in
      `Ok ([{name; pars}])
    | _ -> `Error (sprintf
                     "don't know what to do with %s, see help for details" str)


  let conv : t Arg.conv = parser,printer

end

module Limit_arg = struct
  open Job.Limit

  type t = int * quantity

  let printer fmt (n, q) =
    Format.fprintf fmt "%d %s" n (string_of_quantity q)

  let chop_suffix str suf = match suf with
    | None -> Some str
    | Some suffix -> String.chop_suffix str ~suffix

  let nums = Str.regexp "[0-9]+"

  let parser s =
    let error =
      `Error (sprintf  "string '%s' doesn't fit to limit format" s) in
    if Str.string_match nums s 0 then
      if Str.match_beginning () = 0 then
        let num = Str.matched_string s in
        match String.chop_prefix ~prefix:num s with
        | None | Some "" -> error
        | Some suf ->
           match quantity_of_string suf with
           | Some q -> `Ok (int_of_string num,q)
           | None -> error
      else error
    else error

  let conv : t Arg.conv = parser,printer
end


let doc = "Bap toolkit"

let man = [
  `S "SYNOPSIS";
  `Pre "
      $(mname) --artifacts=... --recipes=...
      $(mname) --artifacts=... --recipes=... --confirmations=...
      $(mname) --schedule=...
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

let info = Term.info ~version:"dev" ~man ~doc "bap-tookit"

let schedule =
  let doc = "creates a schedule of artifacts and recipes to run
             from a provided file, that contains s-expressions in
             the form:
             (artifact1 (recipe1 recipe2))
             (artifact2 recipe1)
             (artifact3 all)
             ... " in
  Arg.(value & opt (some string) None & info ~doc ["schedule"; "s"])

let strings = Arg.(list string)

let artifacts =
  let doc = "A comma-separated list of artifacts to check.
             Every artifact is either a file in the system
             or a TAG from binaryanalysisplatform/bap-artifacts
             docker image" in
  Arg.(value & opt strings [] & info ["artifacts"; "a"] ~doc)

let recipes : Recipes.t list Term.t =
  let doc = "a comma-separated list of the recipes to run.
             A special key $(i,all) can be used to run all the recipes.
             A recipe with parameters should be set individually with
             the same arg:
             --recipes=r1 --recipes=r2:par1=val1,par2=val2
             OR
             -r r1 -r r2:par1=val1,par2=val2 -r r3" in
  Arg.(value & opt_all Recipes.conv [] & info ["recipes"; "r"] ~doc)

let confirms =
  let doc = "file with confirmations. " in
  Arg.(value & opt (some non_dir_file) None & info ["confirmations"; "c"] ~doc)

let output =
  let doc = "file with results" in
  Arg.(value & opt string "results.html" & info ["output"; "o"] ~doc)

let list_recipes =
  let doc = "prints the list of available recipes and exits" in
  Arg.(value & flag & info ["list-recipes"] ~doc)

let list_artifacts =
  let doc = "prints list of available artifacts and exits" in
  Arg.(value & flag & info ["list-artifacts"] ~doc)

let of_incidents =
  let doc = "create a report from file with incidents" in
  Arg.(value & opt (some non_dir_file) None & info ["of-incidents"; "i"] ~doc)

let tool =
  let default = "binaryanalysisplatform/bap-toolkit:latest" in
  let doc = "A tool used to run analysis (default is binaryanalysisplatform/bap-toolkit).
             Tags could be fed as expected, with ':' separator" in
  Arg.(value & opt string default & info ["tool"; "t"] ~doc)

let view =
  let doc = "use a view file with view for rendering incidents" in
  Arg.(value & opt (some non_dir_file) None & info ["view"; "v"] ~doc)

let store =
  let doc = "store results in the file" in
  Arg.(value & opt (some string) None & info ["store"] ~doc)

let update =
  let doc = "update file with results (e.g. run another analysis)" in
  Arg.(value & flag & info ["update"] ~doc)

let of_file =
  let doc = "create a report from previously stored data" in
  Arg.(value & opt (some string) None & info ["from"; "-f"] ~doc)

let limits =
  let doc =
    "Set a memory/time limit per running recipe.
     Job will be canceled if a limit exceeded be canceled.
     Possible limitations:
      time:
        10s - 10 seconds
        10m - 10 minutes
        10h - 10 hours
      memory:
        10Mb - 10 Megabytes
        10Gb - 10 Gigabytes" in
  Arg.(value & opt_all Limit_arg.conv [] & info ["limit"; ] ~doc)
