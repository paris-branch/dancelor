open Html

type t

val pagination : t -> Pagination.t
val search_bar : t -> Model.Any.t SearchBar.t

val make :
  ?initial_input: string ->
  unit ->
  t

val render :
  ?on_input: (string -> unit) ->
  ?attached_buttons: [< Html_types.div_content_fun >`I `Input] elt list ->
  t ->
  [> Html_types.div] elt
