(** {1 Sum of components} *)

val make :
  label: string ->
  ('value, 'raw_value) Component.s list ->
  int option * 'raw_value list ->
  ('value, int option * 'raw_value list) Component.t Lwt.t

exception PartialBecauseWrapped of string
(** The action of wrapping a component is inherently a very partial one.
    Normally, if this Plus component is implemented correctly, and if one gives
    to {!make} wrapped components that complete each other, there should not be
    any issues. Seeing this exception is a bad omen. The string carries some
    details on where this was raised from. *)

val wrap :
  ('value1 -> 'value2) ->
  ('value2 -> 'value1 option) ->
  ('raw_value1 -> 'raw_value2) ->
  ('raw_value2 -> 'raw_value1 option) ->
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) Component.s

(** {2 Internal use} *)

val prepare :
  label: string ->
  ('value, 'raw_value) Component.s list ->
  ('value, int option * 'raw_value list) Component.s
