open Core_kernel
open Bap_report.Std

module IO = struct
  open Bin_prot

  let write ch arti =
    let buf = Utils.bin_dump ~header:true Artifact.bin_writer_t arti in
    Out_channel.output_string ch (Bigstring.to_string buf)


  let read_from_channel chan buf ~pos ~len =
    let s = Bytes.create len in
    match In_channel.really_input chan ~buf:s ~pos:0 ~len with
    | None -> raise End_of_file
    | Some () ->
      Bigstring.From_bytes.blito ~src:s ~dst:buf ~dst_pos:pos ()

  let read ch =
    try
      let read = read_from_channel ch in
      let value = Utils.bin_read_stream ~read Artifact.bin_reader_t in
      Some (Ok value)
    with
    | End_of_file -> None
    | exn -> Some (Error (Error.of_exn exn))

end

let update_db m artis =
  let try_update m a =
    let name = Artifact.name a in
    match Map.find m name with
    | None -> Ok (Map.set m name a)
    | Some a' ->
      match Artifact.merge a a' with
      | Some a -> Ok (Map.set m name a)
      | None ->
        Or_error.errorf "can't update db, got a conflict for %s"
          name in
  let rec update m = function
    | [] -> Ok m
    | a :: artis ->
      match try_update m a with
      | Ok m -> update m artis
      | Error er -> Error er in
  update m artis

let read file =
  let rec loop acc ch =
    match IO.read ch with
    | Some Ok a -> loop (a :: acc) ch
    | Some Error _ -> loop acc ch
    | None -> List.rev acc in
  let artis = In_channel.with_file file
      ~f:(fun ch -> loop [] ch) in
  let m = Map.empty (module String) in
  match update_db m artis with
  | Error er ->
    eprintf "can't rad db: %s\n" (Error.to_string_hum er);
    []
  | Ok m -> Map.data m

let save file artifacts =
  Out_channel.with_file file
    ~f:(fun ch -> List.iter artifacts ~f:(IO.write ch))

let dump ?(update=false) file artifacts =
  let old =
    if update then read file
    else [] in
  let m = Map.empty (module String) in
  match Or_error.(update_db m old >>= fun m -> update_db m artifacts) with
  | Error er ->
    let tmp = Filename.temp_file ~temp_dir:(Sys.getcwd ()) "dump" "bin" in
    eprintf "%s, will store to %s\n" (Error.to_string_hum er) tmp;
    save tmp artifacts
  | Ok m -> save file (Map.data m)
