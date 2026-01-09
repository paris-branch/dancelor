open Nes
open Html

type 'p pagination_mode =
  | Pagination of 'p
  | Fixed_slice of Slice.t

type 'result t

val search_bar : 'result t -> 'result Search_bar.t

val make :
  ?initial_input: string ->
  search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
  pagination_mode: unit pagination_mode ->
  ?min_characters: int ->
  ?on_input: (string -> unit) ->
  ?on_enter: (string -> unit) ->
  unit ->
  'result t

val render :
  make_result: (context: Common.Endpoints.Page.context S.t -> 'result -> Html_types.tr Html.elt) ->
  ?attached_buttons: [< Html_types.div_content_fun >`I `Input] elt list ->
  ?show_table_headers: bool ->
  'result t ->
  [> Html_types.div] elt

module Quick : sig
  type 'result t

  val text : 'result t -> string S.t

  val search_bar : 'result t -> 'result Search_bar.t

  val make :
    search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
    ?on_enter: (string -> unit) ->
    unit ->
    'result t

  val render :
    return: ('dialog_result option -> unit) ->
    dialog_title: string Lwt.t ->
    ?dialog_buttons: Html_types.div_content_fun elt list ->
    make_result: (context: Common.Endpoints.Page.context S.t -> 'result -> Html_types.tr Html.elt) ->
    'result t ->
    Page.t Lwt.t
end
