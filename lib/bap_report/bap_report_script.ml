open Core_kernel
open Bap_report_types
open Bap_report_utils
open Bap_report_read

module Recipe = Bap_report_recipe
module Limit  = Bap_report_limit

type recipe = Recipe.t
type limit  = Limit.t
type journal = string

let mytime = "mytime"
let stdout = "bap.stdout"
let stderr = "bap.stderr"

module Journal = struct
  type t = journal

  let write dir = sprintf "tar czf %s.tgz %s" dir dir

  let tar_list tar =
    match cmd "tar -ztf %s" tar with
    | None -> []
    | Some s ->
      List.filter_map ~f:(fun x ->
          let x = String.strip x in
          if String.is_empty x then None
          else Some x) @@
      String.split ~on:'\n' s

  let tar_exists ~file tar =
    List.exists (tar_list tar) ~f:(fun s -> String.equal s file)

  let read_tar ?target_dir target_file tar read =
    if Sys.file_exists tar then
      let dir = Filename.remove_extension tar in
      let path = match target_dir with
        | None -> sprintf "%s/%s" dir target_file
        | Some dir' -> sprintf "%s/%s/%s" dir dir' target_file in
      if tar_exists ~file:path tar then
        let _ = cmd "tar xzf %s %s" tar path in
        if Sys.file_exists path then
          let r = read path in
          Sys.remove path;
          Option.iter target_dir ~f:(fun dir' -> Unix.rmdir (sprintf "%s/%s" dir dir'));
          Unix.rmdir dir;
          r
        else None
      else None
    else None

  let read ?target_dir target_file tar read =
    try read_tar ?target_dir target_file tar read
    with _ -> None

  let incidents tar =
    let read f = Some (In_channel.with_file f ~f:Bap_report_read.incidents) in
    match read_tar "incidents" tar read with
    | None -> []
    | Some incs -> incs

  let errors tar =
    match read_tar ~target_dir:"log" "log" tar Log.of_file with
    | None -> []
    | Some log -> Log.errors log

  let time tar =
    match read_tar mytime tar Time.of_file with
    | None -> None
    | Some tm -> Time.elapsed tm

end


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
  | Write of journal
  | RunIt of run



let string_of_run {path; recipe; watch; verbose} =
  String.concat ~sep:" " [
    if watch then sprintf "/usr/bin/time -v -o %s" mytime else "";
    "bap";
    path;
    sprintf "--recipe=%s" (Recipe.to_string recipe);
    if verbose
    then
      "-dbir:out.bir --print-bir-attr=address -dasm:out.asm -dogre:out.ogre -dknowledge:out.knowledge" else "";
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
        | Write jrn -> [Journal.write jrn]
        | RunIt run -> [string_of_run run] in
      loop (insns @ acc) xs in
  let header = "#!/usr/bin/env sh\n"; in
  loop [header] insns |> String.concat ~sep:"\n"

let create ~limit ~verbose ~watch ~pwd ~workdir ~path recipe =
  render [
    Limit limit;
    Chdir pwd;
    Mkdir workdir;
    Chdir workdir;
    RunIt {verbose;watch;recipe;path=sprintf "%s/%s" pwd path};
    Chdir pwd;
    Write workdir;
    Rmdir workdir
  ]

let create
    ?(limit=Limit.empty)
    ?(verbose=true)
    ?(watch=true)
    ~pwd
    ~workdir ~path recipe =
  render [
    Limit limit;
    Chdir pwd;
    Mkdir workdir;
    Chdir workdir;
    RunIt {verbose;watch;recipe;path=sprintf "%s/%s" pwd path};
    Chdir pwd;
    Write workdir;
    Rmdir workdir
  ], workdir ^ ".tgz"
