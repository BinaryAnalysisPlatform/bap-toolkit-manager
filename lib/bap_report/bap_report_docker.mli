
open Bap_report_types

type t

val run_recipe : artifact -> string -> t option

val time_taken : t -> float

val find_artifact : string -> artifact option


val toolkit_exists : unit -> bool
