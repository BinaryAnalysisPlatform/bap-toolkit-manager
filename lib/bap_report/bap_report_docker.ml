open Core_kernel
open Bap_report_utils

let available () =
  match cmd "docker images" with
  | None | Some "" -> []
  | Some r ->
     match String.split ~on:'\n' r with
     | [] | [_] -> []
     | _header :: images ->
        List.filter_map images ~f:(fun im ->
            match split_string im with
            | name :: tag :: _ ->
               let tag = match tag with
                 | "<none>" -> None
                 | tag -> Some tag in
               Some (name,tag)
            | _ -> None)

let image_exists ?tag image =
  available () |>
    List.exists
      ~f:(fun (image', tag') ->
        if String.equal image image' then
          match tag, tag' with
          | None,_ -> true
          | Some tag, Some tag' -> String.equal tag tag'
          | _ -> false
        else false)

let stop id =
  ignore @@ cmd "docker container stop %s" id

let remove id =
  ignore @@ cmd "docker container rm %s" id

let clean () =
  match cmd "docker ps --last 1" with
  | None -> ()
  | Some s ->
     match String.split ~on:'\n' s with
     | _header :: container :: _ ->
        let id = match split_string container with
          | id :: _ -> id
          | _ -> "" in
        stop id;
        remove id
     | _ -> ()

let run ~image ?tag ?entry ?mount cmd' =
  let image = with_tag ?tag image in
  let mount = match mount with
    | None -> ""
    | Some (host,guest) -> sprintf "-v %s:%s" host guest in
  let entry = match entry with
    | None -> ""
    | Some e -> sprintf "--entrypoint %s" e in
  let r = cmd "docker run -ti %s %s %s %s" mount entry image cmd' in
  clean ();
  r


let pull ?tag image =
  let image = with_tag ?tag image in
  cmd "docker pull %s" image |> ignore
