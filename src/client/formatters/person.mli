open Common
open Html

val name :
  Model.Person.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt

val name' :
  ?link: bool ->
  Model.Person.t Entry.t ->
  [> `A of [> Html_types.txt] | `PCDATA] elt
(** Variant of {!name} taking an {!Entry.t}. Because this is an entry, we can
    additionnally have a link. *)

val names :
  ?short: bool ->
  Model.Person.t list ->
  [> `A of [> Html_types.txt] | `PCDATA] elt list

val names' :
  ?short: bool ->
  ?links: bool ->
  Model.Person.t Entry.t list ->
  [> `A of [> Html_types.txt] | `PCDATA] elt list
(** Variant of {!names} taking {!Entry.t}. Because they are entries, we can
    additionnally have a link. *)
