open Core_kernel
open Bap_report_types

let read_sexp ch =
  try
    Sexp.input_sexp ch |> Option.some
  with _ -> None

let read_artifact_results arti data =
  let update arti status check incident_data =
    try
      let status = status_of_sexp status in
      let check  = check_of_sexp check in
      let data   = List.map incident_data ~f:Sexp.to_string in
      Artifact.update arti check data status
    with _ -> arti in
  let rec read arti = function
    | [] -> arti
    | x :: xs ->
       match x with
       | Sexp.List (status :: Sexp.List (check :: incident_data) :: _) ->
          read (update arti status check incident_data) xs
       | _ -> read arti xs in
  read arti data

let read_confirmations ch =
  let rec read acc ch =
    match read_sexp ch with
    | None -> acc
    | Some s ->
       match s with
       | Sexp.List (Sexp.Atom _ :: []) -> read acc ch
       | Sexp.List (Sexp.Atom arti :: data) ->
          let acc =
            Map.update acc arti ~f:(function
                | None ->
                   let arti = Artifact.create arti in
                   read_artifact_results arti data
                | Some arti -> read_artifact_results arti data) in
          read acc ch
       | _ -> read acc ch in
  read (Map.empty (module String)) ch

let parse_confirmations file =
  In_channel.with_file file ~f:(fun ch -> read_confirmations ch)

let read file =
  if not (Sys.file_exists file) then []
  else Map.data (parse_confirmations file)
