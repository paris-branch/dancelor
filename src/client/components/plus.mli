(** {1 Sum of components} *)

val make :
  label: string ->
  ('value, 'raw_value) Component.s list ->
  int option * 'raw_value list ->
  ('value, int option * 'raw_value list) Component.t

val wrap :
  ('value1 -> 'value2) ->
  ('raw_value1 -> 'raw_value2) ->
  ('raw_value2 -> 'raw_value1 option) ->
  ('value1, 'raw_value1) Component.s ->
  ('value2, 'raw_value2) Component.s

(** {2 Internal use} *)

val prepare :
  label: string ->
  ('value, 'raw_value) Component.s list ->
  ('value, int option * 'raw_value list) Component.s
