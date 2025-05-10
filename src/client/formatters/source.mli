open Common
open Html

val name : Model.Source.t -> [> Html_types.txt] elt

val name' :
  ?link: bool ->
  Model.Source.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)
