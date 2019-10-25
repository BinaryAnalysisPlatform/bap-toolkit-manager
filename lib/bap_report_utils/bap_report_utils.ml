open Core_kernel

exception Command_failed of Unix.process_status

let cmd fmt =
  let run c =
    try
      let inp = Unix.open_process_in c in
      let res = In_channel.input_all inp in
      match Unix.close_process_in inp with
      | Unix.WEXITED 0 -> Some res
      | s -> raise (Command_failed s)
    with e ->
      eprintf "command '%s' failed: %s\n" c (Exn.to_string e);
      None in
  ksprintf run fmt
