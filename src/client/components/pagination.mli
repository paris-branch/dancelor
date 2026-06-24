(** {1 Page Navigation} *)

open Nes
open Html

type t
(** The abstract type of a page navigation. *)

val create :
  ?initial_page: int ->
  ?on_page_change: (int -> unit) ->
  ?page_url: (int -> Uri.t S.t) ->
  number_of_entries: int option React.signal ->
  entries_per_page: int ->
  unit ->
  t
(** Create a page navigation. Takes a number of entries (or [None] if it is not known at that point)
    and a number of entries per page. [?page_url] allows attaching an actual link to each button.
    It will not actually be followed but can be useful eg. for robots. *)

val render : is_below: bool -> t -> [> Html_types.nav] elt
(** HTML rendering of a page navigation. *)

val slice : t -> Slice.t React.signal
(** Signal giving a {!Model.Slice.t} out of a page navigation. *)

val reset : t -> unit
(** Reset pagination, going back to the first page. *)

val placeholder : is_below: bool -> unit -> [> Html_types.nav] elt
(** HTML rendering of a page navigation placeholder. *)
