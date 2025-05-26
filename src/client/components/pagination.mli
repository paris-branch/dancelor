(** {1 Page Navigation} *)

open Nes
open Html

type t
(** The abstract type of a page navigation. *)

val create :
  number_of_entries: int option React.signal ->
  entries_per_page: int ->
  t
(** Create a page navigation. Takes a number of entries (or [None] if it is not
    known at that point) and a number of entries per page. *)

val render : is_below: bool -> t -> [> Html_types.nav] elt
(** HTML rendering of a page navigation. *)

val slice : t -> Slice.t React.signal
(** Signal giving a {!Model.Slice.t} out of a page navigation. *)

val placeholder : is_below: bool -> unit -> [> Html_types.nav] elt
(** HTML rendering of a page navigation placeholder. *)
