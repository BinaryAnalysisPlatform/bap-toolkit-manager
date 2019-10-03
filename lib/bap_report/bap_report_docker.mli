open Core_kernel

module Image : sig

  type t

  val of_string : string -> t Or_error.t
  val of_string_exn : string -> t

  val to_string : t -> string

  val name : t -> string
  val tag  : t -> string option

  val with_tag : t -> string -> t

  val exists : t -> bool

  val get : t -> unit Or_error.t

  val pull : t -> unit

  val tags : t -> string list

  val name : t -> string
end

type image = Image.t

val run : ?entry:string ->
          ?mount:string * string ->
          image ->
          string ->
          string option
