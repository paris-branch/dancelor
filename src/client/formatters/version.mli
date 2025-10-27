open Nes
open Common
open Html

val description :
  ?arranger_links: bool ->
  Model.Version.t ->
  [> Html_types.span] elt

val description' :
  ?arranger_links: bool ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

val name :
  Model.Version.t ->
  [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?context: Endpoints.Page.context ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val name_disambiguation_and_sources :
  ?params: Model.VersionParameters.t ->
  Model.Version.t ->
  [> Html_types.span] elt

val name_disambiguation_and_sources' :
  ?name_link: bool ->
  ?context: Endpoints.Page.context ->
  ?params: Model.VersionParameters.t ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name_disambiguation_and_sources} taking an {!Entry.t}. Because
    this is an entry, we can additionnally have a link on the name. *)

val disambiguation_and_sources :
  ?source_links: bool ->
  Model.Version.t ->
  [> Html_types.span] elt

val disambiguation_and_sources' :
  ?source_links: bool ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

val composer_and_arranger :
  ?short: bool ->
  ?arranger_links: bool ->
  ?params: Model.VersionParameters.t ->
  Model.Version.t ->
  [> Html_types.span] elt

val composer_and_arranger' :
  ?short: bool ->
  ?arranger_links: bool ->
  ?params: Model.VersionParameters.t ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!composer_and_arranger} taking an {!Entry.t}. *)

val kind_and_structure :
  Model.Version.t ->
  [> Html_types.span] elt

val kind_and_structure' :
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

(** {2 Lifted from tune formatter} *)

val tune_aka :
  Model.Version.t ->
  [> Html_types.span] elt

val tune_aka' :
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

val tune_description :
  Model.Version.t ->
  [> Html_types.span] elt

val tune_description' :
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

val id' :
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

(** {2 Helpers for other models} *)

val names :
  Model.Version.t NEList.t ->
  [> Html_types.span] elt
(** Produces eg. “Tune1, Tune2 & Tune3”. *)

val names' :
  ?links: bool ->
  Model.Version.t Entry.t NEList.t ->
  [> Html_types.span] elt
(** Variant of {!names} taking {!Entry.t}. Because they are entries, we can
    additionnally have links. *)

val names_disambiguations_and_sources :
  (Model.Version.t * Model.VersionParameters.t) NEList.t ->
  [> Html_types.span] elt
(** Produces eg. “Name1 (disamb1) (source1), Name2 & Name3 (disamb3)”. *)

val names_disambiguations_and_sources' :
  ?name_links: bool ->
  (Model.Version.t Entry.t * Model.VersionParameters.t) NEList.t ->
  [> Html_types.span] elt
(** Variant of {!names_disambiguations_and_sources} taking {!Entry.t}. Because
    they are entries, we can additionnally have links on the names. *)

val composers_and_arrangers :
  ?short: bool ->
  ?arranger_links: bool ->
  (Model.Version.t * Model.VersionParameters.t) NEList.t ->
  [> Html_types.span] elt

val composers_and_arrangers' :
  ?short: bool ->
  ?arranger_links: bool ->
  (Model.Version.t Entry.t * Model.VersionParameters.t) NEList.t ->
  [> Html_types.span] elt
(** Variant of {!composers_and_arrangers} taking {!Entry.t}. *)
