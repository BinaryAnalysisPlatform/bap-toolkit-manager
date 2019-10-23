open Bap_report_types

type recipe = Bap_report_recipe.t
type limit  = Bap_report_limit.t
type journal

module Journal : sig
  type t = journal
  val incidents : t -> Bap_report_types.incident list
  val errors : t -> string list
  val time : t -> float option
end

val create :
  ?limit:limit ->
  ?verbose:bool ->
  ?watch:bool ->
  pwd:string ->
  workdir:string ->
  path:string ->
  recipe -> string * journal
