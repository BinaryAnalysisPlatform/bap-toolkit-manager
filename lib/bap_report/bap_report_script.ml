open Core_kernel
open Bap_report_types
open Bap_report_env
module Recipe = Bap_report_recipe
module Limit = Bap_report_limit

type recipe = Recipe.t
type limit  = Limit.t

type run = {
  path    : string;
  recipe  : recipe;
  watch   : bool;
  verbose : bool;
}

type insn =
  | Chdir of string
  | Mkdir of string
  | Rmdir of string
  | Limit of limit
  | TarIt of string
  | RunIt of run

let string_of_run {path; recipe; watch; verbose} =
  String.concat ~sep:" " [
    if watch then sprintf "/usr/bin/time -v -o %s" mytime else "";
    "bap";
    sprintf "%s/%s" drive path;
    sprintf "--recipe=%s" (Recipe.to_string recipe);
    if verbose then "-d -dasm" else "";
    sprintf "> %s 2> %s" stdout stderr;
  ]

let render insns =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs ->
      let insns = match x with
        | Chdir dir -> [sprintf "cd %s" dir]
        | Mkdir dir -> [sprintf "mkdir %s" dir]
        | Rmdir dir -> [sprintf "rm -r %s" dir]
        | Limit lmt -> [Limit.string_of_t lmt]
        | TarIt dir -> [sprintf "tar czf %s.tgz %s" dir dir]
        | RunIt run -> [string_of_run run] in
      loop (insns @ acc) xs in
  let header = "#!/usr/bin/env sh\n"; in
  loop [header] insns |> String.concat ~sep:"\n"

let create
    ?(limit=Limit.empty)
    ?(verbose=true)
    ?(watch=true)
    ~workdir ~path recipe =
  render [
    Limit limit;
    Chdir drive;
    Mkdir workdir;
    Chdir workdir;
    RunIt {verbose;watch;recipe;path};
    Chdir drive;
    TarIt workdir;
    Rmdir workdir
  ]
