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
  ?value: 'value ->
  Html_types.label_content_fun elt list ->
  'value choice
(** Make one choice from the value it must hold (or uses [None]), whether it
    should be checked at the start (defaults to [false]) and its HTML
    contents. *)

(** {2 Choices element} *)

type 'value t
(** Abstract type for a “choices” component that can take ['value]s. *)

val make :
  'value choice list ->
  'value t
(** Make a “choices” component out of a list of single choices. *)

val signal : 'value t -> 'value option S.t
(** A signal holding the value of the “choices” component. *)

val render : 'value t -> Html_types.div elt
(** Render the “choices” component as HTML. *)
