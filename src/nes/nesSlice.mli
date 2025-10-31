(** {1 Slice} *)

type t [@@deriving yojson]
(** Abstract type of slices. *)

val make :
  ?start: int ->
  ?length: int ->
  ?end_incl: int ->
  ?end_excl: int ->
  unit ->
  t
(** Make a slice from bounds. Start bound is always inclusive. End bound can be
    specified as inclusive or exclusive or by the length of the slice. When
    unspecified, starts at [0] and ends at [max_int] included. Raises
    {!Invalid_argument} if the end exclusive of the slice is strictly smaller
    than the start, or if more than one of [end_incl], [end_excl] and
    [length] are specified. *)

val nothing : t
(** The slice that contains nothing. *)

val everything : t
(** The slice that contains everything. *)

val start : t -> int
val end_excl : t -> int
(** Getters from a slice. [end_excl] may return [min_int] to represent an
    inclusive bound up to [max_int]. *)

val list : ?strict: bool -> t -> 'a list -> 'a list
(** Slice a list. Raises {!Invalid_argument} if [start] is strictly bigger than
    the length of the list. If [strict] is set (the default), also raises
    {!Invalid_argument} if [end_excl] is strictly bigger than the length of the
    list; otherwise, silently include everything until the end of the list. *)
