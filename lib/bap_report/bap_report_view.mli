open Core_kernel
open Bap_report_types

type col =
  | Path of int (* deep  *)
  | Name
  | Addr
  | Locations
[@@deriving sexp]

type info =
  | Web of string
  | Col of col
  | Alias of string
[@@deriving sexp]

val register : incident_kind -> info -> unit
val name : incident_kind -> string
val web  : incident_kind -> string option
val tab_of_incident : incident -> string list
