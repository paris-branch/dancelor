open Common
open Html

val name : Model.Tune.t -> [> Html_types.span] elt
val name' : ?link: bool -> Model.Tune.t Entry.t -> [> Html_types.span] elt

val composers' : ?short: bool -> Model.Tune.t Entry.t -> [> Html_types.span] elt

val description : Model.Tune.t -> [> Html_types.span] elt
val description' : Model.Tune.t Entry.t -> [> Html_types.span] elt

val aka : Model.Tune.t -> [> Html_types.span] elt
val aka' : Model.Tune.t Entry.t -> [> Html_types.span] elt
