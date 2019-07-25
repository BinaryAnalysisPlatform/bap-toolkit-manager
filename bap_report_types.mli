open Core_kernel

type artifact
type result = string list

type status =
  | Confirmed
  | False_pos
  | False_neg
  | Undecided
[@@deriving sexp]

type stat = {
    false_pos : int;
    false_neg : int;
    confirmed : int;
    undecided : int;
  }

type check =
  | Unused_return_value
  | Null_ptr_deref
  | Forbidden_function
  | Complex_function
  | Non_structural_cfg
  | Recursive_function
  | Hardcoded_socket_address
  | Memcheck_double_release
  | Memcheck_out_of_bound
  | Memcheck_use_after_release
  | Untrusted_argument
[@@deriving bin_io, compare, sexp]


module Artifact : sig

  val create : ?size:int -> string -> artifact

  val update : artifact -> check -> result -> status -> artifact

  val find_check  : artifact -> check -> (result * status) list
  val find_status : artifact -> check -> result -> status option

  val checks : artifact -> (check * (result * status) list) list

  val name   : artifact -> string

  val size   : artifact -> int option
  val size_hum :  artifact -> string option
  val with_size : artifact -> int -> artifact

  val time : artifact -> check -> float option
  val time_hum : artifact -> check -> string option
  val with_time : artifact -> check -> float -> artifact

  val summary : artifact -> check -> stat

  val merge : artifact -> artifact -> artifact option

end
