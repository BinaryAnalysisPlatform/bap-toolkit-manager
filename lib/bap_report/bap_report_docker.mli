
open Bap_report_types

type t

val run_recipe : artifact -> string -> t

val time_taken : t -> float

val find_artifact : string -> artifact option
