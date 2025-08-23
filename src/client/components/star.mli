(** {1 List of components} *)

open Nes
open Html

val make :
  label: string ->
  ?more_actions: Html_types.div_content_fun elt list S.t ->
  ('value, 'state) Component.s ->
  'state list ->
  ('value list, 'state list) Component.t Lwt.t
(** Make a list component, that is a component that contains 0, 1, or more
    instances of the same sub-component. It contains as value the list of values
    of the sub-components. Note that it consume the sub-components as
    {!Component.s}. *)

val make_non_empty :
  label: string ->
  ?more_actions: Html_types.div_content_fun elt list S.t ->
  ('value, 'state) Component.s ->
  'state list ->
  ('value NEList.t, 'state list) Component.t Lwt.t
(** Variant of {!make} for a list component that has to contain at least one
    sub-component. The value type is therefore {!NonEmptyList.t}. *)

(** {2 Internal use} *)

val prepare :
  label: string ->
  ?more_actions: Html_types.div_content_fun elt list S.t ->
  ('value, 'state) Component.s ->
  ('value list, 'state list) Component.s
(** Variant of {!make} that only prepares the component. It must still be
    {!Component.initialise}d. This is used for composition. *)

val prepare_non_empty :
  label: string ->
  ?more_actions: Html_types.div_content_fun elt list S.t ->
  ('value, 'state) Component.s ->
  ('value NEList.t, 'state list) Component.s
(** Variant of {!make_non_empty} that only prepares the component. It must still
    be {!Component.initialise}d. *)
