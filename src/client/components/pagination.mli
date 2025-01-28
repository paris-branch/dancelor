(** {1 Page Navigation} *)

open Html

type t
(** The abstract type of a page navigation. *)

val create :
  number_of_entries: int option React.signal ->
  entries_per_page: int ->
  t
(** Create a page navigation. Takes a number of entries (or [None] if it is not
    known at that point) and a number of entries per page. *)

val render : t -> [> Html_types.div] elt
(** HTML rendering of a page navigation. *)

val slice : t -> Model.Slice.t React.signal
(** Signal giving a {!Model.Slice.t} out of a page navigation. *)
