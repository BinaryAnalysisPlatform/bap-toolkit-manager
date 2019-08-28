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


let dump ?(update=false) file artifacts =
  Out_channel.with_file file ~append:update
    ~f:(fun ch -> List.iter artifacts ~f:(IO.write ch))

let read file =
  let rec loop acc ch =
    match IO.read ch with
    | Some Ok a -> loop (a :: acc) ch
    | Some Error _ -> loop acc ch
    | None -> List.rev acc in
  In_channel.with_file file
    ~f:(fun ch -> loop [] ch )
