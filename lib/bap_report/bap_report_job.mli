open Bap_report_types

type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image

module Limit : sig
  type quantity = [ `S | `M | `H | `Mb | `Gb ]
  type t

  val empty : t

  val add  : t -> int -> quantity -> t

  val quantity_of_string : string -> quantity option
  val string_of_quantity : quantity -> string

end

type limit = Limit.t

type t


val run  : recipe -> tool:image -> ?image:image -> ?limit:limit -> string -> t
val time : t -> float

val incidents : t -> incident list
val errors : t -> string list list
