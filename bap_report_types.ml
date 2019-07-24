open Core_kernel

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
    took_time : string;
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

type entry  = string list * status
type result = entry list

module Check = struct
  type t = check [@@deriving bin_io, compare, sexp]
  include Comparator.Make(struct
      type t = check [@@deriving bin_io, compare, sexp]
    end)
end
