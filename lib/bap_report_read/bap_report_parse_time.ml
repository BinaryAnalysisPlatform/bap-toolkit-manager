open Core_kernel


type info =
  | Command
  | User_time
  | Sys_time
  | Cpu
  | Time
  | Maximum_res_mem
  | Average_res_mem
  | Exit_status
[@@deriving sexp,bin_io,compare,hash]


module Info = struct

  type t = info
  [@@deriving sexp,bin_io,compare,hash]

  module T = struct
    type nonrec t = t [@@deriving bin_io, compare, hash, sexp]

    let module_name = "Bap_report.Std.Read.Time"
    let to_string id = Sexp.to_string (sexp_of_t id)
    let of_string s = t_of_sexp @@ Sexp.of_string s
  end

  include Identifiable.Make(T)
end

type t = string Info.Map.t

let info_of_string s =
  let (!) = Option.some in
  match String.split ~on:' ' s with
  | "Command" :: _ -> !Command
  | "Exit" :: "status" :: _ -> !Exit_status
  | "User" :: "time" :: _ -> !User_time
  | "System" :: "time" :: _ -> !Sys_time
  | "Elapsed" :: _ -> !Time
  | "Maximum" :: "resident" :: _ -> !Maximum_res_mem
  | "Average" :: "resident" :: _ -> !Average_res_mem
  | "Percent" :: "of" :: "CPU" :: _ -> ! Cpu
  | _ -> None

let parse line =
  let line = String.strip line in
  match String.index line ':' with
  | None -> None
  | Some i ->
    let info = String.subo ~pos:0 ~len:i line in
    match info_of_string info with
    | None -> None
    | Some info ->
      let data = String.subo ~pos:(i+1) line in
      Some (info,String.strip data)

let of_file file =
  if Sys.file_exists file then
    Option.some @@
    In_channel.with_file file ~f:(fun ch ->
        List.fold (In_channel.input_lines ch)
          ~init:(Map.empty (module Info))
          ~f:(fun m line ->
            match parse line with
            | None -> m
            | Some (info,data) -> Map.set m info data))
  else None

let command t = Map.find t Command

let int_of_string x =
  Option.try_with (fun () -> int_of_string x)

let float_of_string x =
  Option.try_with (fun () -> float_of_string x)

let float_of_time xs =
  let (>>=) = Option.bind in
  match xs with
  | [h; m; s] ->
     float_of_string h >>= fun h ->
     float_of_string m >>= fun m ->
     float_of_string s >>= fun s ->
     Some (h *. 60. *. 60. +. m *. 60. +. s)
  | [m; s] ->
     float_of_string m >>= fun m ->
     float_of_string s >>= fun s ->
     Some (m *. 60. +. s)
  | [s] -> float_of_string s
  | _ -> None

let parse_time_fmt_1 s =
  String.split ~on:':' s |> float_of_time

let parse_time_fmt_2 xs =
  let (>>=) = Option.bind in
  let without_suf s ~suf = String.chop_suffix s ~suffix:suf in
  match xs with
  | [h; m; s] ->
     without_suf h "h" >>= fun h ->
     without_suf m "m" >>= fun m ->
     without_suf s "s" >>= fun s ->
     float_of_time [h;m;s]
  | [m; s] ->
     without_suf m "m" >>= fun m ->
     without_suf s "s" >>= fun s ->
     float_of_time [m;s]
  | [s] ->
     without_suf s "s" >>= fun s ->
     float_of_time [s]
  | _ -> None

let parse_time str =
  match String.strip str |> String.split ~on:' ' with
  | [str] -> parse_time_fmt_1 str
  | xs -> parse_time_fmt_2 xs

let user_time t =
  Option.(Map.find t User_time >>= float_of_string)

let system_time t =
  Option.(Map.find t Sys_time >>= float_of_string)

let cpu_percentage t =
  Option.(
    Map.find t Cpu >>= fun str ->
    String.filter str ~f:(fun c -> c <> '%') |> String.strip |>
    int_of_string)

let elapced t =
  Option.(Map.find t Time >>= parse_time)
