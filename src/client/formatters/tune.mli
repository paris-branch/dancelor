open Dancelor_common
open Html

val name : Model.Tune.t -> [> Html_types.span] elt

val name' :
  ?link: bool ->
  ?in_search: Endpoints.Page.In_search.t S.t ->
  Model.Tune.entry ->
  [> Html_types.span] elt

val composers' :
  ?short: bool ->
  ?links: bool ->
  Model.Tune.entry ->
  [> Html_types.span] elt

val description : Model.Tune.t -> [> Html_types.span] elt
val description' : Model.Tune.entry -> [> Html_types.span] elt

val aka : Model.Tune.t -> [> Html_types.span] elt
val aka' : Model.Tune.entry -> [> Html_types.span] elt
