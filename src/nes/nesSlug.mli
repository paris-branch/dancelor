(** {1 Slug} *)

(** Type of slug carrying values of type ['value]. *)
type 'value t
[@@deriving yojson]

val none : 'any t
val is_none : 'any t -> bool

val equal : ('any -> 'any -> bool) -> 'any t -> 'any t -> bool
(** For compatibility with [ppx_deriving.std]'s [equal]. Prefer {!equal'}. *)

val equal' : 'any t -> 'any t -> bool
val compare : 'any t -> 'any t -> int

val from_string : string -> 'any t
(** Creates a slug from the given string. This function fails with
    [Invalid_argument _] if the given string is empty. The returned slug is never
    [none]. *)

val check_string : string -> 'any t option
(** Check that the string is already in slug form, and return a corresponding
    slug. Return [None] otherwise. *)

exception NotASlug

val check_string_exn : string -> 'any t
(** Same as {!check_string} but raises {!NotASlug}. *)

val to_string : 'any t -> string
(** Returns a string representing the slug. This function fails with [Failure _]
    [none]. *)

val pp : (Format.formatter -> 'any -> unit) -> Format.formatter -> 'any t -> unit
(** For debugging purposes with [ppx_deriving_show]. Prefer {!pp'}. *)

val pp' : Format.formatter -> 'any t -> unit

(** {2 Low-level and Unsafe} *)

val unsafe_coerce : 'a t -> 'b t
(** Loses the type information from the slug. *)

val unsafe_equal : 'a t -> 'b t -> bool
(** Compare while losing type information. *)

val unsafe_of_string : string -> 'any t
(** Take the given string as a slug as-is. Using [to_string] followed by
    [unsafe_of_string] allows to change the type associated to the slug, which
    can violate interface properties. This should be avoided. FIXME: superseeded
    by [check_string]? *)

val compare_slugs_or : fallback: ('a -> 'a -> int) -> ('a -> 'a t) -> 'a -> 'a -> int
(** Compare two objects by their slug if they are defined. If at least one of
    the two slugs is undefined, falls back on given comparison function. *)
