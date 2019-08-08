open Core_kernel
open Bap_report_utils

module type A = sig
  val tags : string -> string list
  val image : string -> bool
end

module Net_available = struct

  let tags image =
    match cmd "wget -q https://registry.hub.docker.com/v1/repositories/%s/tags -O -" image with
    | None -> []
    | Some s ->
       let s = String.filter s ~f:(fun c -> c <> '"') in
       let s = String.tr s ~target:'}' ~replacement:' ' in
       let s = String.split ~on:' ' s in
       fst @@
         List.fold s ~init:([],false)
           ~f:(fun (acc,remember) w ->
             if remember then w :: acc, false
             else if w = "name:" then acc,true
             else acc,false)

  let image name =
    let name' =
      match String.split name ~on:'/' with
      | [_repo;name] -> name
      | _ -> name in
    match cmd "docker search %s" name' with
    | None -> false
    | Some s ->
       match String.split ~on:'\n' s with
       | _header :: names ->
          let names = List.filter_map names
                        ~f:(fun s -> split_string s |> List.hd) in
          List.mem names name ~equal:String.equal
       | _ -> false

end

module Loc_available = struct

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

  let tags image =
    List.fold (available ()) ~init:[] ~f:(fun acc (im,tag) ->
        if String.equal image im then
          match tag with
          | None -> acc
          | Some tag -> tag :: acc
        else acc)

  let image name =
    List.exists (available ()) ~f:(fun (im,_) -> String.equal name im)

end

module Exists(A : A) = struct
  let test ?tag name =
    match tag with
    | None -> A.image name
    | Some tag -> List.mem (A.tags name) tag ~equal:String.equal
end

let exists_locally ?tag image =
  let module E = Exists(Loc_available) in
  E.test ?tag image

let exists_globaly ?tag image =
  let module E = Exists(Net_available) in
  E.test ?tag image

let available_tags image =
  match Net_available.tags image with
  | [] -> Loc_available.tags image
  | tags -> tags

let image_exists ?tag name =
  exists_locally ?tag name || exists_globaly ?tag name

let pull ?tag image =
  let image = with_tag ?tag image in
  cmd "docker pull %s" image |> ignore

let get_image ?tag name =
  if exists_locally ?tag name then Ok ()
  else
    if exists_globaly ?tag name then
      let () = pull ?tag name in
      if exists_locally ?tag name then Ok ()
      else
        Or_error.errorf "can't pull image %s" (with_tag ?tag name)
    else Or_error.errorf "can't detect image %s" (with_tag ?tag name)


let stop id = ignore @@ cmd "docker container stop %s" id

let remove id = ignore @@ cmd "docker container rm %s" id

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
