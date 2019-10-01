open Bap_report_types

type recipe = Bap_report_recipe.t
type image = Bap_report_docker.image
type limit = Bap_report_limit.t

type t

val run  : recipe -> tool:image -> ?verbose:bool -> ?image:image -> ?limit:limit -> string -> t
val time : t -> float
val incidents : t -> incident list
val errors : t -> string list
