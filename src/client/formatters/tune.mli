open Common
open Html

val name : Model.Tune.t -> [> `A of [> Html_types.txt] | `PCDATA] elt
val name' : ?link: bool -> Model.Tune.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt

val composers : ?short: bool -> Model.Tune.t -> [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t
val composers' : ?short: bool -> Model.Tune.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t

val description : Model.Tune.t -> [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t
val description' : Model.Tune.t Entry.t -> [> `A of [> Html_types.txt] | `PCDATA] elt list Lwt.t

val aka : Model.Tune.t -> [> Html_types.txt] Html.elt list
val aka' : Model.Tune.t Entry.t -> [> Html_types.txt] Html.elt list
