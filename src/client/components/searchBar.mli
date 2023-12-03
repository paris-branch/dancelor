(** {1 SearchBar} *)

open Js_of_ocaml_tyxml.Tyxml_js

(** Abstraction of the possible states of the search bar. *)
type 'result state =
  | StartTyping (** when the user has not typed anything yet *)
  | ContinueTyping (** when the user has not typed enough yet *)
  | NoResults (** when the search returned no results *)
  | Results of 'result list (** when the search returned results; guaranteed to be non empty; otherwise [NoResults] *)
  | Errors of string list (** when the search returned an error; guaranteed to be non empty *)

(** Type of a search bar; contains all that is useful to interact with it. *)
type ('result, 'html) t = {
  html : ([> Html_types.input] as 'html) Html.elt;
  state : 'result state React.S.t;
  set_text : (string -> unit);
}

val make :
  placeholder:string ->
  search:(Dancelor_client_model.Pagination.t -> string -> ('result list, string list) result Lwt.t) ->
  ?on_focus:(unit -> unit) ->
  ?on_enter:(string -> unit) ->
  ?autofocus:bool ->
  ?min_characters:int ->
  pagination:Dancelor_client_model.Pagination.t React.signal ->
  unit ->
  ('result, 'html) t
(** Makes a search bar and exposes whatever is useful to interact with it. The
    arguments are to be used as follows:

    - [placeholder] shows in the bar when no text is entered yet;

    - [search] is an Lwt function that returns either a list of results or a
      list of error messages to display. It should respect the given pagination.

    - [on_focus] is a function that fires when the bar gains focus;

    - [on_enter] is a function that triggers when the user presses Enter.

    - [autofocus] is a boolean that indicates whether the search bar should grab
      the focus when the page loads. It is [false] by default.

    - [min_characters] is an integer indicating the minimum number of characters
      to type for the search bar to fire. By default, there is no such limit.

    - [pagination] is a signal giving the state of the pagination.
*)

val quick_search :
  placeholder:string ->
  search:(Dancelor_client_model.Pagination.t -> string -> ('result list, string list) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  ?on_enter:(string -> unit) ->
  ?autofocus:bool ->
  unit ->
  [> Html_types.div] Html.elt
(** Wrapper around {!make} returning a quick search bar. This is a search bar
    with a floating table. The table reports hints or errors and shows the
    search results. The minimum number of characters is set to [3] and the
    pagination picks only one page of [10] entries.

    The arguments are mostly inherited from {!make}; refer to this function for
    documentation. The additional ones are to be used as follows:

    - [make_result] is a function that, from a result, returns a table line
      displaying that result;
*)
