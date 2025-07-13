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
  Model.Version.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val name_and_dance :
  ?dance_link: bool ->
  Model.Version.t ->
  Model.VersionParameters.t ->
  [> Html_types.span] elt

val name_and_dance' :
  ?name_link: bool ->
  ?dance_link: bool ->
  Model.Version.t Entry.t ->
  Model.VersionParameters.t ->
  [> Html_types.span] elt
(** Variant of {!name_and_dance} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)

val name_disambiguation_and_sources :
  Model.Version.t ->
  [> Html_types.span] elt

val name_disambiguation_and_sources' :
  ?name_link: bool ->
  Model.Version.t Entry.t ->
  [> Html_types.span] elt
(** Variant of {!name_disambiguation_and_sources} taking an {!Entry.t}. Because
    this is an entry, we can additionnally have a link on the name. *)

val disambiguation_and_sources :
  Model.Version.t ->
  [> Html_types.span] elt

val disambiguation_and_sources' :
  Model.Version.t Entry.t ->
  [> Html_types.span] elt

val composer_and_arranger :
  ?short: bool ->
  ?arranger_links: bool ->
  Model.Version.t ->
  [> Html_types.span] elt

val composer_and_arranger' :
  ?short: bool ->
  ?arranger_links: bool ->
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
