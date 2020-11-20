open Core_kernel

type entry =
  | Bap_error

type t = (entry * string list) list

let message_of_string line =
  let ended_with_more str =
    String.(is_suffix (strip str) ~suffix:">") in
  match String.split line ~on:' ' with
  | source :: _ when ended_with_more source ->
    let data =
      String.(strip (chop_prefix_exn ~prefix:source line)) in
    let source =
      String.(chop_suffix_exn ~suffix:">" (strip source)) in
    Some (source,data)
  | _ -> None

let is_bap_error str = String.(str = "bap.error")

let predicates = [is_bap_error, Bap_error]

let read ch =
  let add ready acc = function
    | None -> ready
    | Some e ->(e, acc) :: ready in
  let find_known s =
    List.find_map predicates ~f:(fun (f, e) ->
        if f s then Some e
        else None) in
  let rec loop ready entry acc =
    match In_channel.input_line ch with
    | None -> List.rev (add ready acc entry)
    | Some line ->
      match message_of_string line with
      | None ->
        if Option.is_some entry then
          loop ready entry (line :: acc)
        else loop ready entry acc
      | Some (src,data) ->
        match find_known src with
        | None -> loop (add ready acc entry) None acc
        | entry' ->
          loop (add ready acc entry) entry' [data] in
  loop [] None []

let of_file log =
  if Sys.file_exists log then
    Some (In_channel.with_file log ~f:read)
  else None

let errors t =
  List.fold t ~init:[] ~f:(fun acc (Bap_error,data) ->
      data::acc) |> List.rev |> List.concat
