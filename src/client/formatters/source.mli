open Common
open Html

val name :
  Model.Source.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  Model.Source.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)
