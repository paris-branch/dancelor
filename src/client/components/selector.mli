(** {1 Selector component} *)

open Nes
open Common
open Html

val make :
  label: string ->
  search: (Slice.t -> string -> (int * ('model, 'access) Entry.t list, string) Result.t Lwt.t) ->
  unserialise: ('model Entry.id -> ('model, 'access) Entry.t option Lwt.t) ->
  make_descr: (('model, 'access) Entry.t -> string Lwt.t) ->
  make_result:
  (?classes: string list ->
  ?action: Utils.Result_row.action ->
  ?prefix: Utils.Result_row.cell list ->
  ?suffix: Utils.Result_row.cell list ->
  ('model, 'access) Entry.t ->
  Utils.Result_row.t) ->
  ?make_more_results:
  (('model, 'access) Entry.t ->
  Utils.Result_row.t list S.t) ->
  model_name: string ->
  ?create_dialog_content: ((('model, 'access) Entry.t, 'any) Editor.mode -> Page.t Lwt.t) ->
  'model Entry.id option ->
  (('model, 'access) Entry.t, 'model Entry.id option) Component.t Lwt.t
(** When [?create_dialog_content] is passed, an additional button allows to
    create a value of this type on the fly. *)

(** {2 Internal use} *)

val prepare :
  label: string ->
  search: (Slice.t -> string -> (int * ('model, 'access) Entry.t list, string) Result.t Lwt.t) ->
  unserialise: ('model Entry.Id.t -> ('model, 'access) Entry.t option Lwt.t) ->
  make_descr: (('model, 'access) Entry.t -> string Lwt.t) ->
  make_result:
  (?classes: string list ->
  ?action: Utils.Result_row.action ->
  ?prefix: Utils.Result_row.cell list ->
  ?suffix: Utils.Result_row.cell list ->
  ('model, 'access) Entry.t ->
  Utils.Result_row.t) ->
  ?make_more_results:
  (('model, 'access) Entry.t ->
  Utils.Result_row.t list S.t) ->
  model_name: string ->
  ?create_dialog_content: ((('model, 'access) Entry.t, 'any) Editor.mode -> Page.t Lwt.t) ->
  unit ->
  (('model, 'access) Entry.t, 'model Entry.Id.t option) Component.s
(** Variant of {!make} that only prepares the component. It must still be
    {!Component.initialise}d. This is used for composition with eg.
    {!ComponentList}. *)
