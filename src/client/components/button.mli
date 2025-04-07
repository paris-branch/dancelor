open Html

val make :
  ?label: string ->
  ?label_processing: string ->
  ?icon: string ->
  ?badge: string ->
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

val make_icon :
  ?classes: string list ->
  string ->
  [> Html_types.button] elt
(** Make a fake button showing an icon. Sometimes useful in button groups. *)

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
  onclick: (unit -> unit Lwt.t) ->
  unit ->
  [> Html_types.button] elt
(** A button specialised in cancelling something. *)

val cancel' :
  return: ('any option -> unit) ->
  unit ->
  [> Html_types.button] elt
(** Variant of {!cancel'} passing [None] to a [return] function. *)
