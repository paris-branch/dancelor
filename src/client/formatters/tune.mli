open Common
open Html

val name : Model.Tune.t -> [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?context: Endpoints.Page.context S.t ->
  Model.Tune.entry ->
  [> Html_types.span] elt

val composers' : ?short: bool -> Model.Tune.entry -> [> Html_types.span] elt

val description : Model.Tune.t -> [> Html_types.span] elt
val description' : Model.Tune.entry -> [> Html_types.span] elt

val aka : Model.Tune.t -> [> Html_types.span] elt
val aka' : Model.Tune.entry -> [> Html_types.span] elt
