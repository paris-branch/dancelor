(** {1 Choices}

    This module helps building a “choices” component, which shows multiple
    choices horizontally, one of which can be selected. This would show as
    something like:

        Book Dance Person [Set] Tune Version

    This is similar to a <select> element except that all the choices are
    visible. Under the hood, it is a bunch of <radio> elements.
*)

open Html

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
(** Variant of {!val-choice} that holds the option of a value. Useful when the
    default choice is to do nothing. *)

(** {2 Choices element} *)

val make_radios :
  label: string ->
  'value option choice list ->
  ('value option, string) Component.t
(** Make a radio-based “choices” component that can hold at most one value at
    once out of a list of single choices. *)

val make_radios' :
  label: string ->
  validate: ('choice_value option -> ('value, string) Result.t) ->
  'choice_value option choice list ->
  ('value, string) Component.t
(** Variant of {!make_radios} with a validation function. *)

val make_checkboxes :
  label: string ->
  'value choice list ->
  ('value list, string) Component.t
(** Make a checkbox-based “choices” component that can hold zero, one, or
    several values at once out of a list of single choices. *)

(** {2 Internal use} *)

val prepare_radios' :
  label: string ->
  validate: ('choice_value option -> ('value, string) Result.t) ->
  'choice_value option choice list ->
  ('value, string) Component.s

val prepare_checkboxes :
  label: string ->
  'value choice list ->
  ('value list, string) Component.s
