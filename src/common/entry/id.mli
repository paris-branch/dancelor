(** {1 Entry.Id}

    Global identifier for Dancelor's database. They are composed of three groups
    of four alphanumerical characters, one of which is a control sum. This makes
    for 36 ** 11, ie. over 10 ** 17 or over 2 ** 56 ids. *)

(** Type of ids carrying values of type ['value]. *)
type 'value t
[@@deriving yojson]

val make : unit -> 'any t
(** Make a new random id. *)

val equal : ('any -> 'any -> bool) -> 'any t -> 'any t -> bool
(** For compatibility with [ppx_deriving.std]'s [equal]. Prefer {!equal'}. *)

val compare : ('any -> 'any -> int) -> 'any t -> 'any t -> int
(** For compatibility with [ppx_deriving.std]'s [compare]. Prefer {!compare'}. *)

val equal' : 'any t -> 'any t -> bool
val compare' : 'any t -> 'any t -> int

val of_string : string -> 'any t option
(** Creates an id from the given string, or returns [None] if the given string
    is not a valid id. *)

val of_string_exn : string -> 'any t
(** Creates an id from the given string. This function fails with
    [Invalid_argument _] if the given string is not a valid id. *)

val to_string : 'any t -> string
(** Returns a string representing the id. *)

val of_yojson' : Yojson.Safe.t -> ('any t, string) result

val to_yojson' : 'any t -> Yojson.Safe.t

val pp : (Format.formatter -> 'any -> unit) -> Format.formatter -> 'any t -> unit
(** For debugging purposes with [ppx_deriving_show]. Prefer {!pp'}. *)

val pp' : Format.formatter -> 'any t -> unit

(** {2 Low-level and Unsafe} *)

val unsafe_coerce : 'a t -> 'b t
(** Loses the type information from the id. *)

val unsafe_equal : 'a t -> 'b t -> bool
(** Compare while losing type information. *)

(** {2 Serialisation} *)

module type TYPEABLE = sig type t end

module S (A : TYPEABLE) : Madge.STRINGABLE with type t = A.t t

module J (A : TYPEABLE) : Madge.JSONABLE with type t = A.t t
