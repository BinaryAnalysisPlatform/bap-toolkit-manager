

module Std : sig

  module Docker : sig

    (** [image_exists image ~tag] returns true if
      the [image] with [tag] exists. Returns false
      otherwise (e.g. if tag is set, but image with such
      tag is not found. *)
    val image_exists : ?tag:string -> string -> bool


    (** [available ()] returns the list of all available images
      in the user system with theirs tags (if exist) as well. *)
    val available : unit -> (string * string option) list

    (** [run ~image ~tag ~entry ~mount cmd] runs [cmd] with the docker
      [image].

      @param mount defines a mounting volume as a pair of pathes
             in the host and in the image.

      @param entry override an entrypoint for the [image] *)
    val run : image:string -> ?tag:string -> ?entry:string ->
              ?mount:string * string -> string -> string option

    (** [pull image ~tag] pulls the [image] with [tag] *)
    val pull : ?tag:string -> string -> unit

  end


  module Recipe : sig

    type t

    (** [find name] finds a recipe by its name. Return None if
      not recipe found *)
    val find : string -> t option

    (** [list () ] returns the list of available recipes  *)
    val list : unit -> t list

    (** [name recipe] returns the name of the recipe  *)
    val name : t -> string

    (** [description recipe]returns the description of the recipe *)
    val description : t -> string

    (** [run ~image ~tag path recipe] runs the recipe.
      if [image] and/or [tag] is set then [path] is considered
      relatively to the [image], else to the host filesystem  *)
    val run : ?image:string -> ?tag:string -> string -> t -> t

    (** [time_taken recipe] returns a time that was spent for
      the last {run} command *)
    val time_taken : t -> float

  end

  (** [size ?image ?tag path] returns the size of the file at [path].
      if [image] and/or [tag] is set then [path] is considered
      relatively to the [image], else to the host filesystem*)
  val size : ?image:string -> ?tag:string -> string -> int option


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
   }[@@deriving sexp]

  type result = string list
  [@@deriving bin_io,compare, sexp]

  type artifact

  module Artifact : sig

    type t = artifact

    (** [create ~size name] creates a new artifact. *)
    val create : ?size:int -> string -> t

    (** [update artifact check result status]
        updates the [artifact] with new data *)
    val update : t -> check -> result -> status -> t

    (** [find_result artifact check] returns a list of results
        for the [check] *)
    val find_result  : t -> check -> (result * status) list

    (** [checks artifact] returns all the checks that were run
        against [artifact] *)
    val checks : t -> check list

    (** [name artifact] returns a name of the [artifact] *)
    val name   : t -> string

    (** [size artifact] return a size (in bytes) of the [artifact]*)
    val size   : t -> int option

    (** [size_hum artifact] returns a human readable string
        with size of the [artifact] *)
    val size_hum :  t -> string option

    (** [with_size artifact size] updates a size of the [artifact]*)
    val with_size : t -> int -> t

    (** [time artifact check] returns a time taken to run
        [check] against [artifact] in seconds *)
    val time : t -> check -> float option

    (** [time_hum artifact check] returns a time taken to run
        [check] against [artifact] in human readable
        format *)
    val time_hum : t -> check -> string option

    (** [with_time artifact check time] updates time *)
    val with_time : t -> check -> float -> t

    (** [summary artifact check] returns a summary for the given [check] *)
    val summary : t -> check -> stat

    (** [merge artifact artifact'] merge artifacts data.
        returns None if some certaion contradictions found, e.g.
        when one tries to merge artifacts with different
        names. *)
    val merge : t -> t -> t option

  end


  module Incidents : sig

    (** [process : artifact file] read incidents from [file]
        and updates [artifact] *)
    val process : artifact -> string -> artifact

  end

  module Confirmations : sig

    (** [read file] reads file with confirmations and
        returns the list of artifacts with checks
        that have some confirmed status *)
    val read : string -> artifact list
  end


  module Template : sig

    (** [render artifacts] retutns the html report  *)
    val render : artifact list -> string
  end

end
