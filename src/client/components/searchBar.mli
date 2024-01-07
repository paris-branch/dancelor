(** {1 SearchBar} *)

open Js_of_ocaml_tyxml.Tyxml_js

(** Abstraction of the possible states of the search bar. *)
type 'result state =
  | StartTyping (** when the user has not typed anything yet *)
  | ContinueTyping (** when the user has not typed enough yet *)
  | NoResults (** when the search returned no results *)
  | Results of 'result list (** when the search returned results; guaranteed to be non empty; otherwise [NoResults] *)
  | Errors of string list (** when the search returned an error; guaranteed to be non empty *)

type 'result t
(** Abstract type of a search bar, holding results of type ['result]. *)

val state : 'result t -> 'result state React.signal
(** Signal giving a state out of a search bar. *)

val make :
  search:(Dancelor_client_model.Pagination.t -> string -> (int * 'result list, string list) result Lwt.t) ->
  ?min_characters:int ->
  pagination:Dancelor_client_model.Pagination.t React.signal ->
  ?on_number_of_entries:(int -> unit) ->
  ?initial_input: string ->
  unit ->
  'result t
(** Makes a search bar and exposes whatever is useful to interact with it. The
    arguments are to be used as follows:

    - [search] is an Lwt function that returns either a list of results or a
      list of error messages to display. It should respect the given pagination
      and return the total number of entries that match the query.

    - [min_characters] is an integer indicating the minimum number of characters
      to type for the search bar to fire. By default, there is no such limit.

    - [pagination] is a signal giving the state of the pagination.

    - [on_number_of_entries] is a function that fires whenever the [search]
      returns a number of entries.

    - [initial_input] is a string specifying the initial input of the search
      bar. This string will appear in the search bar and a first search will be
      triggered with it.
*)

val render :
  placeholder:string ->
  ?autofocus:bool ->
  ?on_focus:(unit -> unit) ->
  ?on_input:(string -> unit) ->
  ?on_enter:(string -> unit) ->
  'result t ->
  [> Html_types.input] Html.elt
(** Renders a search bar as an HTML node. The arguments are to be used as
    follows:

    - [placeholder] shows in the bar when no text is entered yet;

    - [on_focus] is a function that fires when the bar gains focus;

    - [on_input] is a function that triggers whenever there is an input.

    - [on_enter] is a function that triggers when the user presses Enter.

    - [autofocus] is a boolean that indicates whether the search bar should grab
      the focus when the page loads. It is [false] by default.
*)

val quick_search :
  placeholder:string ->
  search:(Dancelor_client_model.Pagination.t -> string -> (int * 'result list, string list) result Lwt.t) ->
  make_result:('result -> Html_types.tr Html.elt Lwt.t) ->
  ?on_enter:(string -> unit) ->
  ?autofocus:bool ->
  unit ->
  [> Html_types.div] Html.elt
(** Wrapper around {!make} and {!render} returning a quick search bar. This is a
    search bar with a floating table. The table reports hints or errors and
    shows the search results. The minimum number of characters is set to [3] and
    the pagination picks only one page of [10] entries.

    The arguments are mostly inherited from {!make} and {!render}; refer to
    these functions for documentation. The additional ones are to be used as
    follows:

    - [make_result] is a function that, from a result, returns a table line
      displaying that result;
*)
