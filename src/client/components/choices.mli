(** {1 Choices}

    This module helps building a “choices” component, which shows multiple
    choices horizontally, one of which can be selected. This would show as
    something like:

        Book Dance Person [Set] Tune Version

    This is similar to a <select> element except that all the choices are
    visible. Under the hood, it is a bunch of <radio> elements.
*)

open Dancelor_client_html

(** {2 Single choice} *)

type 'value choice
(** Abstract type for one choice holding a single ['value]. *)

val choice :
  ?checked: bool ->
  value: 'value ->
  Html_types.label_content_fun elt list ->
  'value choice
(** Make one choice from the value it must hold, whether it should be checked at
    the start (defaults to [false]) and its HTML contents. *)

val choice' :
  ?checked: bool ->
  ?value: 'value ->
  Html_types.label_content_fun elt list ->
  'value option choice
(** Variant of {!choice} that holds the option of a value. Useful when the
    default choice is to do nothing. *)

(** {2 Choices element} *)

type 'value t
(** Abstract type for a “choices” component that can take ['value]s. *)

val make_radios :
  'value option choice list ->
  'value option t
(** Make a radio-based “choices” component that can hold at most one value at
    once out of a list of single choices. *)

val make_checkboxes :
  'value choice list ->
  'value list t
(** Make a checkbox-based “choices” component that can hold zero, one, or
    several values at once out of a list of single choices. *)

val signal : 'value t -> 'value S.t
(** A signal holding the value/s of the “choices” component. *)

val value : 'value t -> 'value
(** The value/s of the “choices” component at that point. *)

val render : 'value t -> Html_types.div elt
(** Render the “choices” component as HTML. *)
