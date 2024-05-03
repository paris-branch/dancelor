(** {1 Input components} *)

open React
open Js_of_ocaml_tyxml.Tyxml_js

(** {2 Text input components} *)

module Text : sig
  type 'a t
  (** Abstract type of a text input component. *)

  val make :
    ?has_interacted: bool S.t ->
    string ->
    (string -> ('a, string) Result.t) ->
    'a t
  (** Make a text input component from the initial input string and a validator
      function. Can receive an optional [?has_interacted] signal that describes
      whether someone interacted with the global form. The interaction impacts
      whether errors are reported or not. *)

  val raw_signal : 'a t -> string S.t
  (** A signal to the raw value of the input text component. *)

  val signal : 'a t -> ('a, string) Result.t S.t
  (** A signal to the value of the input text component as validated by the
      validator. *)

  val clear : 'a t -> unit
  (** Clear a text input component to an empty value . *)

  val render : ?label:string -> ?placeholder:string -> 'a t -> Html_types.div Html.elt
  (** Render a text input component as HTML. *)

  val render_as_textarea : ?label:string -> ?placeholder:string -> 'a t -> Html_types.div Html.elt
  (** Variant of {!render} that renders the input as a textarea. *)
end
