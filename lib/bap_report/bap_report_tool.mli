open Core_kernel
open Bap_report_types
open Bap_report_docker

type t
type recipe = Bap_report_recipe.t

val of_image : image -> t Or_error.t

val host : unit -> t Or_error.t

val recipes : t -> recipe list

val find_recipe : t -> string -> recipe option

val image : t -> image option

val bap_version : t -> string option

val to_string : t -> string
