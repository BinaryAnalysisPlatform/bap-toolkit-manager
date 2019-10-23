open Bap_report_types

type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image
type limit = Bap_report_limit.t
type path = Bap_report_path.t
type tool = Bap_report_tool.t

type t
type ctxt

val context : ?verbose:bool -> ?limit:limit -> tool -> ctxt

val run : ctxt -> recipe -> path -> t

val time : t -> float
val incidents : t -> incident list
val errors : t -> string list
