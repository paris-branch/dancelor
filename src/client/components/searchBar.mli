(** {1 SearchBar} *)

open Js_of_ocaml_tyxml.Tyxml_js

val quick_search :
  placeholder:string ->
  search:(string -> ('result list, string list) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  max_results:int ->
  ?on_enter:(string -> unit) ->
  ?autofocus:bool ->
  unit ->
  [> Html_types.div] Html.elt
(** Makes a quick search bar. This is an input element with a floating table.
    The table reports hints or errors and shows the search results. The
    arguments are to be used as follows:

    - [placeholder] shows in the bar when no text is entered yet;

    - [search] is an Lwt function that returns either a list of results or a
      list of error messages to display;

    - [make_result] is a function that, from a result, returns a table line
      displaying that result;

    - [max_results] is a threshold on the number of results to display;

    - [on_enter] is a function that triggers when the user presses Enter.

    - [autofocus] is a boolean that indicates whether the search bar should grab
      the focus when the page loads. It is [false] by default.

    A search bar is simply a <div> element.
*)
