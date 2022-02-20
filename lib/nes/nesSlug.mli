(** {1 Slug} *)

(** Type of slug carrying values of type ['value]. *)
type 'value t
[@@deriving yojson]

val none : 'any t
val is_none : 'any t -> bool

val equal : 'any t -> 'any t -> bool
val compare : 'any t -> 'any t -> int

val from_string : string -> 'any t
(** Creates a slug from the given string. This function fails with
   [Invalid_argument _] if the given string is empty. The returned slug is never
   [none]. *)

val to_string : 'any t -> string
(** Returns a string representing the slug. This function fails with [Failure _]
   [none]. *)

val pp : Format.formatter -> 'any t -> unit

(** {2 Low-level and Unsafe} *)

val unsafe_of_string : string -> 'any t
(** Take the given string as a slug as-is. Using [to_string] followed by
   [unsafe_of_string] allows to change the type associated to the slug, which
   can violate interface properties. This should be avoided. *)

val compare_slugs_or :
  fallback:('a -> 'a -> int Lwt.t) ->
  ('a -> 'a t Lwt.t) ->
  'a -> 'a ->
  int Lwt.t
