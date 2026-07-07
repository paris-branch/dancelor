open Dancelor_common
open Html

val name :
  Model.Set.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?params: Model.Set_parameters.t ->
  ?in_search: Endpoints.Page.In_search.t S.t ->
  Model.Set.entry ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val tunes' :
  ?link: bool ->
  Model.Set.entry ->
  [> Html_types.span] elt
(** Variant of {!tunes} taking an {!Entry.t}. *)

val conceptors' :
  ?link: bool ->
  ?short: bool ->
  ?params: Model.Set_parameters.t ->
  Model.Set.entry ->
  [> Html_types.span] elt
(** Variant of {!conceptors} taking an {!Entry.t}. *)
