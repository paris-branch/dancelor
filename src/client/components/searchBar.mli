(** {1 SearchBar} *)

open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_model

(** Abstraction of the possible states of the search bar. *)
type 'result state =
  | StartTyping (** when the user has not typed anything yet *)
  | ContinueTyping (** when the user has not typed enough yet *)
  | NoResults (** when the search returned no results *)
  | Results of 'result list (** when the search returned results; guaranteed to be non empty; otherwise [NoResults] *)
  | Errors of string (** when the search returned an error *)

type 'result t
(** Abstract type of a search bar, holding results of type ['result]. *)

val state : 'result t -> 'result state React.signal
(** Signal giving a state out of a search bar. *)

val text : 'result t -> string React.signal
(** Signal holding the text of the search bar. Prefer relying on {!state} when
    possible. *)

val set_text : 'result t -> string -> unit
(** Imperatively set the text of the search bar. *)

val make :
  search: (Slice.t -> string -> (int * 'result list, string) result Lwt.t) ->
  ?min_characters:int ->
  slice: Slice.t React.signal ->
  ?on_number_of_entries:(int -> unit) ->
  ?initial_input: string ->
  unit ->
  'result t
(** Makes a search bar and exposes whatever is useful to interact with it. The
    arguments are to be used as follows:

    - [search] is an Lwt function that returns either a list of results or a
      list of error messages to display. It should respect the given slice
      and return the total number of entries that match the query.

    - [min_characters] is an integer indicating the minimum number of characters
      to type for the search bar to fire. By default, there is no such limit.

    - [slice] is a signal giving the state of the slice.

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
