(** {1 List of components} *)

open Nes

val make :
  ('value, 'raw_value) Component.s ->
  'raw_value list ->
  ('value list, 'raw_value list) Component.t
(** Make a list component, that is a component that contains 0, 1, or more
    instances of the same sub-component. It contains as value the list of values
    of the sub-components. Note that it consume the sub-components as
    {!Component.s}. *)

val make_non_empty :
  ('value, 'raw_value) Component.s ->
  'raw_value list ->
  ('value NonEmptyList.t, 'raw_value list) Component.t
(** Variant of {!make} for a list component that has to contain at least one
    sub-component. The value type is therefore {!NonEmptyList.t}. *)

(** {2 Internal use} *)

val prepare :
  ('value, 'raw_value) Component.s ->
  ('value list, 'raw_value list) Component.s
(** Variant of {!make} that only prepares the component. It must still be
    {!Component.initialise}d. This is used for composition. *)

val prepare_non_empty :
  ('value, 'raw_value) Component.s ->
  ('value NonEmptyList.t, 'raw_value list) Component.s
(** Variant of {!make_non_empty} that only prepares the component. It must still
    be {!Component.initialise}d. *)
