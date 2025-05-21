open Common
open Html

val name : Model.Dance.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
val name' : ?link: bool -> Model.Dance.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt

val name_and_disambiguation :
  Model.Dance.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] elt list Lwt.t

val name_and_disambiguation' :
  ?name_link: bool ->
  Model.Dance.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA | `Span] elt list Lwt.t
(** Variant of {!name_and_disambiguation} taking an {!Entry.t}. Because this is an entry,
    we can additionnally have a link on the name. *)
