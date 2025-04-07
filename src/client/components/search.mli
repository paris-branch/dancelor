open Nes
open Html

type 'p pagination_mode =
  | Pagination of 'p
  | FixedSlice of Slice.t

type t

val search_bar : t -> Model.Any.t SearchBar.t

val make :
  ?initial_input: string ->
  pagination_mode: unit pagination_mode ->
  ?min_characters: int ->
  unit ->
  t

val render :
  ?on_input: (string -> unit) ->
  ?attached_buttons: [< Html_types.div_content_fun >`I `Input] elt list ->
  ?show_table_headers: bool ->
  t ->
  [> Html_types.div] elt
