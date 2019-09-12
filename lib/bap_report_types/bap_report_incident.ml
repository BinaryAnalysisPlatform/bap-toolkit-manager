open Core_kernel
open Bap_report_common

module Kind = struct
  let of_string x = x
  let to_string x = x
  include String
end

module Locations = struct
  type t = addr list [@@deriving bin_io, compare, hash, sexp]

  let create ?(prev=[]) addr = addr :: prev
  let addrs x = x
  let addr xs = List.hd_exn xs

  module T = struct
    type nonrec t = t [@@deriving bin_io,compare,hash,sexp]
    let module_name = "Bap_report.Std.Locations"
    let to_string locs =
      List.fold ~init:"" locs ~f:(fun s a ->
          sprintf "%s%s " s @@ Addr.to_string a)

    let of_string x =
      String.split x ~on:' ' |>   List.map ~f:Addr.of_string

  end

  include Identifiable.Make(T)

end

type kind = Kind.t [@@deriving bin_io, compare, hash, sexp]
type locations = Locations.t [@@deriving bin_io, compare, hash, sexp]


module Id = struct

  type t = {
    locs : locations;
    kind : kind;
  } [@@deriving bin_io, compare, hash, sexp]

  let create kind locs = {kind;locs}
  let locations t = t.locs
  let kind t = t.kind

  module T = struct
    type nonrec t = t [@@deriving bin_io, compare, hash, sexp]

    let module_name = "Bap_report.Std.Incident_id"
    let to_string id = Sexp.to_string (sexp_of_t id)
    let of_string s = t_of_sexp @@ Sexp.of_string s
  end

  include Identifiable.Make(T)

end

type id = Id.t [@@deriving bin_io, compare, hash, sexp]

type t = {
  id   : id;
  path : string list;
} [@@deriving bin_io, hash, sexp]


let create ?(path=[]) locs kind =
  {id = Id.create kind locs; path}

let locations t = t.id.locs
let path t = t.path
let kind t = t.id.kind
let addr t = Locations.addr t.id.locs

let id t = t.id

let of_id id = create (Id.locations id) (Id.kind id)


let compare t t' =
  let r = Kind.compare t.id.kind t'.id.kind in
  if r <> 0 then r
  else Locations.compare t.id.locs t'.id.locs


module T = struct
  type nonrec t = t [@@deriving bin_io, compare, hash, sexp]

  let module_name = "Bap_report.Std.Incident"
  let to_string id = Sexp.to_string (sexp_of_t id)
  let of_string s = t_of_sexp @@ Sexp.of_string s
end

include Identifiable.Make(T)
