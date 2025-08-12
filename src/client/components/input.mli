(** {1 Input components} *)

open React
open Js_of_ocaml_tyxml.Tyxml_js

type type_ = Text | Password | Textarea

val make :
  type_: type_ ->
  label: string ->
  ?placeholder: string ->
  serialise: ('value -> string) ->
  validate: (string -> ('value, string) result S.t) ->
  ?oninput: (string -> unit) ->
  ?template: string ->
  string ->
  ('value, string) Component.t

val inactive :
  label: string ->
  string ->
  [> Html_types.div] Html.elt
(** An inactive text input, compatible graphically with [make ~type_: Text]. *)

(** {2 Internal use} *)

val prepare :
  type_: type_ ->
  label: string ->
  ?placeholder: string ->
  serialise: ('value -> string) ->
  validate: (string -> ('value, string) result S.t) ->
  ?oninput: (string -> unit) ->
  ?template: string ->
  unit ->
  ('value, string) Component.s
(** Variant of {!make} that only prepares the component. It must still be
    {!Component.initialise}d. This is used for composition with eg.
    {!ComponentList}. *)
