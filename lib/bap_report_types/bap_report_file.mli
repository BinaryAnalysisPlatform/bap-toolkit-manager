open Core_kernel

type image = Bap_report_image.t

type t [@@deriving bin_io, compare, sexp]

val create :  ?image:image -> string -> t

val image : t -> image option

(** returns a path, relatively to the root of
    either host or image, depends on what was fed to [create] *)
val path  : t -> string

val size : t -> int option

val equal : t -> t -> bool
