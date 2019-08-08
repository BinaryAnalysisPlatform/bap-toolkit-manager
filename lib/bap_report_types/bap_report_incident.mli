open Core_kernel
open Bap_report_common

type t [@@deriving compare]

val create :
  ?trace:string list -> ?machine:string -> ?data:string list -> check -> string list -> t

val add_data : t -> string list -> t

val check : t -> check
val trace : t -> string list
val locations : t -> string list
val machine : t -> string option
val data : t -> string list

module Map : Map.S with type Key.t = t
module Set : Set.S with type Elt.t = t
