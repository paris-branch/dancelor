(** {1 Page Navigation} *)

(* FIXME: rename this into [Pagination] (which it is); rename the current
   [Pagination] into [Slice] or something, which describes it better. Maybe we
   don't even need a type for that. *)

open Dancelor_common_model
open Dancelor_client_html

type t
(** The abstract type of a page navigation. *)

val create :
  number_of_entries: int React.signal ->
  entries_per_page: int ->
  t
(** Create a page navigation. Takes a number of entries and a number of entries
    per page. *)

val render : t -> [> Html_types.div ] elt
(** HTML rendering of a page navigation. *)

val pagination : t -> Pagination.t React.signal
(** Signal giving a {!Pagination} out of a page navigation. *)
