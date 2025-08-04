(** {1 Input components} *)

open React
open Js_of_ocaml_tyxml.Tyxml_js

type type_ = Text | Password | Textarea

module type Constants = sig
  type value
  val label : string
  val placeholder : string
  val type_ : type_
  val validator : string -> (value, string) result S.t
  val oninput : string -> unit
end

module Make : functor (X : Constants) ->
  Component.S with
  type value = X.value
  and type raw_value = string

val inactive :
  ?label: string ->
  string ->
  [> Html_types.div] Html.elt
(** An inactive text input, compatible graphically with [make ~type_: Text]. *)
