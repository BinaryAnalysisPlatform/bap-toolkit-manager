open Core_kernel


type info =
  | Command
  | User_time
  | Sys_time
  | Cpu
  | Time
  | Maximum_res_mem
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

let known_prefixes = [
  "Command being timed", Command;
  "User time (seconds)", User_time;
  "System time (seconds)", Sys_time;
  "Percent of CPU this job got", Cpu;
  "Elapsed (wall clock) time (h:mm:ss or m:ss)", Time;
  "Maximum resident set size (kbytes)", Maximum_res_mem;
  "Exit status", Exit_status;
]

let find_info s =
  List.find known_prefixes ~f:(fun (p,_) -> String.is_prefix s ~prefix:p)

let info_of_string s =
  let s = String.strip s in
  match find_info s with
  | None -> None
  | Some (pref, info) ->
    Some (info,
          String.strip @@
          String.subo ~pos:(String.length pref + 1) s)

let of_file file =
  if Sys.file_exists file then
    Option.some @@
    In_channel.with_file file ~f:(fun ch ->
        List.fold (In_channel.input_lines ch)
          ~init:(Map.empty (module Info))
          ~f:(fun m line ->
              match info_of_string line with
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

let elapsed t =
  Option.(Map.find t Time >>= parse_time)
