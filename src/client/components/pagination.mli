(** {1 Page Navigation} *)

open Dancelor_common_model
open Dancelor_client_html

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

val slice : t -> Slice.t React.signal
(** Signal giving a {!Slice} out of a page navigation. *)
