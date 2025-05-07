open Common
open Html

val description :
  ?arranger_links: bool ->
  Model.Version.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t

val description' :
  ?arranger_links: bool ->
  Model.Version.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t

val name : Model.Version.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
val name' : ?link: bool -> Model.Version.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val name_and_dance :
  ?dance_link: bool ->
  Model.Version.t ->
  Model.VersionParameters.t ->
  [> `A of [> Html_types.txt] | `Br | `PCDATA | `Small] Html.elt list Lwt.t

val name_and_dance' :
  ?name_link: bool ->
  ?dance_link: bool ->
  Model.Version.t Entry.t ->
  Model.VersionParameters.t ->
  [> `A of [> Html_types.txt] | `Br | `PCDATA | `Small] Html.elt list Lwt.t
(** Variant of {!name_and_dance} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)

val name_and_disambiguation :
  Model.Version.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] Html.elt list Lwt.t

val name_and_disambiguation' :
  ?name_link: bool ->
  Model.Version.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] Html.elt list Lwt.t
(** Variant of {!name_and_disambiguation} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)

val name_disambiguation_and_sources' :
  ?name_link: bool ->
  Model.Version.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] elt list Lwt.t

val disambiguation_and_sources' :
  Model.Version.t Entry.t ->
  [> `PCDATA | `Span] elt list Lwt.t

val composer_and_arranger :
  ?short: bool ->
  ?arranger_links: bool ->
  Model.Version.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] elt list Lwt.t

val composer_and_arranger' :
  ?short: bool ->
  ?arranger_links: bool ->
  Model.Version.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] elt list Lwt.t
(** Variant of {!composer_and_arranger} taking an {!Entry.t}. *)
