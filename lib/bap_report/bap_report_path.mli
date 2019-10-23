open Bap_report_docker

type t

val create :  ?image:image -> string -> t

val image : t -> image option

(** return a path, relatively to the root of
    either host or image, depends on what was fed to [create] *)
val relative  : t -> string

val size : t -> int option
