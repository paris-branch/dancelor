open Nes
open Common
open Html

val name :
  Model.Version.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?context: Endpoints.Page.context ->
  Model.Version.entry ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val disambiguation :
  ?parentheses: bool ->
  Model.Version.t ->
  [> Html_types.span] elt

val disambiguation' :
  ?parentheses: bool ->
  Model.Version.entry ->
  [> Html_types.span] elt

val name_disambiguation_and_sources' :
  ?name_link: bool ->
  ?context: Endpoints.Page.context ->
  ?params: Model.Version_parameters.t ->
  Model.Version.entry ->
  [> Html_types.span] elt
(** Variant of {!name_disambiguation_and_sources} taking an {!Entry.t}. Because
    this is an entry, we can additionnally have a link on the name. *)

val disambiguation_and_sources' :
  ?parentheses: bool ->
  ?source_links: bool ->
  Model.Version.entry ->
  [> Html_types.span] elt

val composer_and_arranger' :
  ?short: bool ->
  ?arranger_links: bool ->
  ?params: Model.Version_parameters.t ->
  Model.Version.entry ->
  [> Html_types.span] elt
(** Variant of {!composer_and_arranger} taking an {!Entry.t}. *)

val kind_and_structure' :
  Model.Version.entry ->
  [> Html_types.span] elt

(** {2 Lifted from tune formatter} *)

val tune_description' :
  Model.Version.entry ->
  [> Html_types.span] elt

val id' :
  Model.Version.entry ->
  [> Html_types.span] elt

(** {2 Helpers for other models} *)

val names :
  Model.Version.t NEList.t ->
  [> Html_types.span] elt
(** Produces eg. “Tune1, Tune2 & Tune3”. *)

val names' :
  ?links: bool ->
  Model.Version.entry NEList.t ->
  [> Html_types.span] elt
(** Variant of {!names} taking {!Entry.t}. Because they are entries, we can
    additionnally have links. *)

val names_disambiguations_and_sources' :
  ?name_links: bool ->
  (Model.Version.entry * Model.Version_parameters.t) NEList.t ->
  [> Html_types.span] elt
(** Variant of {!names_disambiguations_and_sources} taking {!Entry.t}. Because
    they are entries, we can additionnally have links on the names. *)

val composers_and_arrangers' :
  ?short: bool ->
  ?arranger_links: bool ->
  (Model.Version.entry * Model.Version_parameters.t) NEList.t ->
  [> Html_types.span] elt
(** Variant of {!composers_and_arrangers} taking {!Entry.t}. *)
