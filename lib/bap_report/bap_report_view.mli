open Core_kernel
open Bap_report_types

type col =
  | Path of int (* deep  *)
  | Name
  | Addr
  | Locations

type info =
  | Web of string
  | Tab of col list
  | Alias of string

type t

val create : unit -> t

val update : t -> incident_kind -> info -> unit

val name : t -> incident_kind -> string
val web  : t -> incident_kind -> string option
val data : t -> incident -> string list

val of_file : string -> t
