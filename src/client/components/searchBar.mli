(** {1 SearchBar} *)

open Js_of_ocaml_tyxml.Tyxml_js

val make :
  placeholder:string ->
  search:(string -> ('result list, string list) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  max_results:int ->
  ?on_enter:(string -> unit) ->
  unit ->
  [> Html_types.div] Html.elt
(** Makes a search bar:

    - [placeholder] shows in the bar when no text is entered yet;

    - [search] is an Lwt function that returns either a list of results or a
      list of error messages to display;

    - [make_result] is a function that, from a result, returns a table line
      displaying that result;

    - [max_results] is a threshold on the number of results to display;

    - [on_enter] is a function that triggers when the user presses Enter.

    A search bar is simply a <div> element.
*)
