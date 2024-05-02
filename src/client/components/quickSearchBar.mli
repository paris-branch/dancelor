(** {1 Quick Search Bar}

    This is a wrapper around {!SearchBar}; a search bar with a floating table.
    The table reports hints or errors and shows the search results. The minimum
    number of characters is set to [3] and the slice picks only one page of [10]
    entries. *)

open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_model

type 'result t
(** Abstract type of a quick search bar, holding results of type ['result]. *)

val clear : 'result t -> unit
(** Imperatively clear the quick search bar. *)

val make :
  ?number_of_results: int ->
  search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
  unit ->
  'result t
(** Wrapper around {!SearchBar.make}; refer to it for a description of the
    arguments. *)

val render :
  placeholder: string ->
  make_result: ('result -> Html_types.tr Html.elt Lwt.t) ->
  ?on_enter: (string -> unit) ->
  ?more_lines: Html_types.tr Html.elt list ->
  ?autofocus: bool ->
  'result t ->
  [> Html_types.div] Html.elt
(** Wrapper around {!SearchBar.render}; refer to it for a description of the
    arguments. Additionally, [make_result] is a function that, from a result,
    returns a table line displaying that result. *)

val make_and_render :
  ?number_of_results: int ->
  placeholder:string ->
  search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  ?on_enter:(string -> unit) ->
  ?more_lines: Html_types.tr Html.elt list ->
  ?autofocus:bool ->
  unit ->
  [> Html_types.div] Html.elt
(** Short form of the composition of {!make} directly followed by {!render}. *)

val fa_row :
  ?onclick: (unit -> unit) ->
  string ->
  string ->
  Html_types.tr Html.elt
