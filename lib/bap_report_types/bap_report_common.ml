open Core_kernel

type status =
  | Confirmed
  | False_pos
  | False_neg
  | Undecided
[@@deriving bin_io, compare, sexp]

type stat = {
  false_pos : int;
  false_neg : int;
  confirmed : int;
  undecided : int;
} [@@deriving bin_io, compare, sexp]

type result = string list
[@@deriving bin_io,compare, sexp]

module Addr = Bap_report_addr
type addr = Addr.t [@@deriving bin_io,compare, hash,sexp]
