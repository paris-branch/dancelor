open Common
open Html

val name :
  Model.Dance.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  Model.Dance.t Entry.t ->
  [> Html_types.span] elt

val name_and_disambiguation :
  Model.Dance.t ->
  [> Html_types.span] elt

val name_and_disambiguation' :
  ?name_link: bool ->
  Model.Dance.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name_and_disambiguation} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)
