open Core_kernel


let split s =
  List.filter_map (String.split s ~on:' ')
    ~f:(fun s ->
        match String.strip s with
        | "" -> None
        | s  -> Some s)

let is_comment = function
  | None -> fun _ -> false
  | Some prefix -> fun line -> String.is_prefix ~prefix line

let words ?comments ch =
  let is_comment = is_comment comments in
  let rec read acc = function
    | [] -> List.rev acc
    | line :: lines ->
      match String.strip line with
      | "" -> read acc lines
      | line when is_comment line -> read acc lines
      | line -> read (split line :: acc) lines in
  read [] (In_channel.input_lines ch)

let lines ?comments ch =
  let is_comment = is_comment comments in
  let rec read acc = function
    | [] -> List.rev acc
    | line :: lines ->
      match String.strip line with
      | "" -> read acc lines
      | line when is_comment line -> read acc lines
      | line -> read (line :: acc) lines in
  read [] (In_channel.input_lines ch)
