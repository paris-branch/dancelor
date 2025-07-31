(** {1 Input components} *)

open React
open Js_of_ocaml_tyxml.Tyxml_js

type type_ = Text | Password | Textarea

type 'a t
(** Abstract type of a text input component. *)

val make :
  ?label: string ->
  ?placeholder: string ->
  ?oninput: (string -> unit) ->
  type_: type_ ->
  initial_value: string ->
  validator: (string -> ('a, string) Result.t) ->
  unit ->
  'a t
(** Make a text input component from the initial input string and a validator
    function. *)

val make' :
  ?label: string ->
  ?placeholder: string ->
  ?oninput: (string -> unit) ->
  type_: type_ ->
  initial_value: string ->
  validator: (string -> ('a, string) Result.t S.t) ->
  unit ->
  'a t
(** Variant of {!make} where the validator gets access to the internal signal.
    In fact, [make x v = make' x (S.const % v)]. *)

val raw_signal : 'a t -> string S.t
(** A signal to the raw value of the input text component. *)

val signal : 'a t -> ('a, string) Result.t S.t
(** A signal to the value of the input text component as validated by the
    validator. *)

val value : 'a t -> ('a, string) Result.t
(** Short for [S.value % signal]. *)

val clear : 'a t -> unit
(** Clear a text input component to an empty value . *)

val focus : 'a t -> unit
(** Focus a text input component. *)

val html : 'a t -> [> Html_types.div] Html.elt
(** Render a text input component as HTML. *)

val inactive :
  ?label: string ->
  string ->
  [> Html_types.div] Html.elt
(** An inactive text input, compatible graphically with [make ~type_: Text]. *)
