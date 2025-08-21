(** {1 Sum of components} *)

val make :
  label: string ->
  ('value, 'state) Component.s list ->
  int option * 'state list ->
  ('value, int option * 'state list) Component.t Lwt.t

exception PartialBecauseWrapped of string
(** The action of wrapping a component is inherently a very partial one.
    Normally, if this Plus component is implemented correctly, and if one gives
    to {!make} wrapped components that complete each other, there should not be
    any issues. Seeing this exception is a bad omen. The string carries some
    details on where this was raised from. *)

val wrap :
  ('value1 -> 'value2) ->
  ('value2 -> 'value1 option) ->
  ('state1 -> 'state2) ->
  ('state2 -> 'state1 option) ->
  ('value1, 'state1) Component.s ->
  ('value2, 'state2) Component.s

(** {2 Internal use} *)

val prepare :
  label: string ->
  ('value, 'state) Component.s list ->
  ('value, int option * 'state list) Component.s
