open Core_kernel


val image_exists : ?tag:string -> string -> bool

val available_tags: string -> string list

val run : image:string -> ?tag:string -> ?entry:string ->
          ?mount:string * string -> string -> string option

val pull : ?tag:string -> string -> unit

val get_image : ?tag:string -> string -> unit Or_error.t
