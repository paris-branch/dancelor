open Common
open Html

val name : Model.Dance.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
val name' : ?link: bool -> Model.Dance.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
