open Core_kernel
open Bap_report_types

type t
type image = Bap_report_docker.image
type recipe = t

val find : image -> string -> t option
val list : image -> t list

val add_parameter : t -> name:string -> value:string -> t

val name : t -> string

val description : t -> string

val to_string : t -> string

module Job : sig
  module Limit : sig
    type quantity = [ `S | `M | `H ]
    type t

    val empty : t

    val add  : t -> int -> quantity -> t

    val quantity_of_string : string -> quantity option
    val string_of_quantity : quantity -> string

  end

  type limit = Limit.t

  type t

  val run : recipe -> tool:image -> ?image:image -> ?limit:limit -> string -> t
  val time : t -> float

  val results : t -> string

  val incidents : t -> incident list

end
