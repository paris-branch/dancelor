(** {1 Input components} *)

open Js_of_ocaml_tyxml.Tyxml_js

(** {2 Text input components} *)

module Text : sig
  type 'a t
  (** Abstract type of a text input component. *)

  val make : string -> (string -> ('a, string) Result.t) -> 'a t
  (** Make a text input component from the initial input string and a validator
      function. *)

  val signal : 'a t -> ('a, string) Result.t React.S.t
  (** A signal to the value of the input text component as validated by the
      validator. *)

  val clear : 'a t -> unit
  (** Clear a text input component to an empty value . *)

  val render : placeholder:string -> 'a t -> Html_types.div Html.elt
  (** Render a text input component as HTML. *)
end
