open Html

val make :
  label: string ->
  label_processing: string ->
  icon: string ->
  classes: string list ->
  ?disabled: bool S.t ->
  onclick: (unit -> unit Lwt.t) ->
  unit ->
  [> Html_types.button] elt
(** Generic button showing [label] and [icon]. When clicked, [label] is replaced
    by [label_processing], and [icon] by a spinning one. The button already has
    classes ["btn"] and ["disabled"] when needed, but others can be added. When
    clicked, the button triggers [onclick] and considers its job done when
    [onclick] returns. It is possible to use [Lwt.async], but it is recommended
    to actually return when that makes sense. *)

val save :
  ?disabled: bool S.t ->
  onclick: (unit -> unit Lwt.t) ->
  unit ->
  [> Html_types.button] elt
(** A button specialised in saving a form. *)

val clear :
  onclick: (unit -> unit) ->
  unit ->
  [> Html_types.button] elt
(** A button specialised in clearing a form. *)

val cancel :
  ?return: ('any option -> unit) ->
  ?onclick: (unit -> unit Lwt.t) ->
  unit ->
  [> Html_types.button] elt
(** A button specialised in cancelling something. One cannot use both [return]
    and [onclick] at the same time. If [return] is provided, the action is to
    call [return None]. *)
