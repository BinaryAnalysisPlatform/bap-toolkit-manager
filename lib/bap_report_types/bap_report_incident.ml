open Core_kernel
open Bap_report_common

type addr = string [@@deriving sexp, compare]

type locs = addr list  [@@deriving sexp, compare]

type aux = {
    mach  : string option;
    trace : string list;
    data  : string list;
} [@@deriving sexp]

type t = {
  check : check;
  locs  : locs;
  aux   : aux;
}[@@deriving sexp]


let create ?(trace=[]) ?machine ?(data=[]) check locs =
  let aux = {mach=machine; trace; data} in
  {check; locs; aux}

let add_data t data =
  {t with aux = {t.aux with data = t.aux.data @ data } }

let trace t = t.aux.trace
let machine t = t.aux.mach
let locations t = t.locs
let data t = t.aux.data
let check t = t.check

let compare t t' =
  let r = compare_check t.check t'.check in
  if r <> 0 then r
  else compare_locs t.locs t'.locs

module Map = Map.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)


module Set = Set.Make(struct
    type nonrec t = t [@@deriving sexp]
    let compare = compare
  end)
