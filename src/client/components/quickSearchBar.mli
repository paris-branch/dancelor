open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_model

val make_and_render :
  ?number_of_results: int ->
  placeholder:string ->
  search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  ?on_enter:(string -> unit) ->
  ?autofocus:bool ->
  unit ->
  [> Html_types.div] Html.elt
(** Wrapper around {!SearchBar.make} and {!SearchBar.render} returning a quick
    search bar. This is a search bar with a floating table. The table reports
    hints or errors and shows the search results. The minimum number of
    characters is set to [3] and the slice picks only one page of [10] entries.

    The arguments are mostly inherited from {!SearchBar.make} and
    {!SearchBar.render}; refer to these functions for documentation. The
    additional ones are to be used as follows:

    - [make_result] is a function that, from a result, returns a table line
       displaying that result;
*)
