open Html

val name :
  Model.Person.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?context: Common.Endpoints.Page.context S.t ->
  Model.Person.entry ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val names :
  ?short: bool ->
  Model.Person.t list ->
  [> Html_types.span] elt

val names' :
  ?short: bool ->
  ?links: bool ->
  Model.Person.entry list ->
  [> Html_types.span] elt
(** Variant of {!names} taking {!Entry.t}. Because they are entries, we can
    additionnally have a link. *)

val names_with_details :
  ?short: bool ->
  (Model.Person.t * Html_types.span_content elt list) list ->
  [> Html_types.span] elt
(** More generic version of {!names} which can add details to each name. *)

val names'_with_details :
  ?short: bool ->
  ?links: bool ->
  (Model.Person.entry * Html_types.span_content elt list) list ->
  [> Html_types.span] elt
(** Variant of {!names'_with_details} taking {!Entry.t}. Because they are entries, we can
    additionnally have a link. *)
