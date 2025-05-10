open Common
open Html

val works :
  Model.Set.t ->
  [> [> Html_types.txt] Html_types.a | Html_types.txt] elt list Lwt.t

val works' :
  Model.Set.t Entry.t ->
  [> [> Html_types.txt] Html_types.a | Html_types.txt] elt list Lwt.t
(** Variant of {!works} taking an {!Entry.t}. *)

val name :
  Model.Set.t ->
  [> [> Html_types.txt] Html_types.a | Html_types.txt] elt

val name' :
  ?link: bool ->
  Model.Set.t Entry.t ->
  [> [> Html_types.txt] Html_types.a | Html_types.txt] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val tunes :
  ?link: bool ->
  Model.Set.t ->
  [> Html_types.span] elt list Lwt.t

val tunes' :
  ?link: bool ->
  Model.Set.t Entry.t ->
  [> Html_types.span] elt list Lwt.t
(** Variant of {!tunes} taking an {!Entry.t}. *)

val name_and_tunes :
  ?tunes_link: bool ->
  Model.Set.t ->
  [> `A of [> Html_types.txt] | Html_types.br | Html_types.txt | Html_types.small] elt list Lwt.t

val name_and_tunes' :
  ?name_link: bool ->
  ?tunes_link: bool ->
  Model.Set.t Entry.t ->
  [> `A of [> Html_types.txt] | Html_types.br | Html_types.txt | Html_types.small] elt list Lwt.t
(** Variant of {!name_and_tunes} taking an {!Entry.t}. Because this is an entry,
    we can additionally have a link on the name. *)

val name_tunes_and_dance :
  ?tunes_link: bool ->
  ?dance_link: bool ->
  Model.Set.t ->
  Model.SetParameters.t ->
  [> `A of [> Html_types.txt] | Html_types.br | Html_types.txt | Html_types.small] elt list Lwt.t

val name_tunes_and_dance' :
  ?name_link: bool ->
  ?tunes_link: bool ->
  ?dance_link: bool ->
  Model.Set.t Entry.t ->
  Model.SetParameters.t ->
  [> `A of [> Html_types.txt] | Html_types.br | Html_types.txt | Html_types.small] elt list Lwt.t
(** Variant of {!name_tunes_and_dance} taking an {!Entry.t}. Because this is an
    entry, we can additionally have a link on the name. *)
