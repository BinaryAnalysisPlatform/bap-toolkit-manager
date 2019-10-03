open Core_kernel
open Bap_report_common

module Kind : sig
  type t [@@deriving bin_io, compare, sexp]
  include Identifiable.S with type t := t
end

module Locations : sig

  type t [@@deriving bin_io, compare, sexp]

  val create : ?prev:addr list -> addr -> t
  val addrs : t -> addr list

  include Identifiable.S with type t := t
end

type locations = Locations.t [@@deriving bin_io, compare, sexp]
type kind = Kind.t [@@deriving bin_io, compare, sexp]

type t [@@deriving bin_io, compare, sexp]


module Id : sig

  type t [@@deriving bin_io, compare, sexp]

  val create : kind -> locations -> t
  val locations : t -> locations
  val kind : t -> kind

  include Identifiable.S with type t := t
end

type id = Id.t  [@@deriving bin_io, compare, sexp]


val create : ?path:string list ->
             locations -> kind -> t

val of_id : id -> t

val addr : t -> addr
val locations : t -> locations
val path : t -> string list
val kind : t -> kind
val id   : t -> Id.t


include Identifiable.S with type t := t
