open Html

val name :
  Model.Dance.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?context: Common.Endpoints.Page.context S.t ->
  Model.Dance.entry ->
  [> Html_types.span] elt

val name_and_disambiguation' :
  ?name_link: bool ->
  ?context: Common.Endpoints.Page.context S.t ->
  Model.Dance.entry ->
  [> Html_types.span] elt
(** Variant of {!name_and_disambiguation} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)

val aka : Model.Dance.t -> [> Html_types.span] elt
val aka' : Model.Dance.entry -> [> Html_types.span] elt
