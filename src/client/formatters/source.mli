open Common
open Html

val name :
  ?short: bool ->
  Model.Source.t ->
  [> Html_types.span] elt

val name' :
  ?short: bool ->
  ?link: bool ->
  Model.Source.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val date_and_editors :
  Model.Source.t ->
  [> Html_types.span] elt

val date_and_editors' :
  Model.Source.t Entry.t ->
  [> Html_types.span] elt
