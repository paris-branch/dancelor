(** {1 Selector component} *)

open Nes
open Common
open Html

val make :
  label: string ->
  search: (Slice.t -> string -> (int * 'model Entry.t list, string) Result.t Lwt.t) ->
  unserialise: ('model Entry.Id.t -> 'model Entry.t option Lwt.t) ->
  make_descr: ('model Entry.t -> string Lwt.t) ->
  make_result:
  (?classes: string list ->
  ?action: Utils.ResultRow.action ->
  ?prefix: Utils.ResultRow.cell list ->
  ?suffix: Utils.ResultRow.cell list ->
  'model Entry.t ->
  Utils.ResultRow.t) ->
  ?make_more_results:
  ('model Entry.t ->
  Utils.ResultRow.t list S.t) ->
  model_name: string ->
  create_dialog_content: (('model Entry.t, 'any) Editor.mode -> Page.t Lwt.t) ->
  'model Entry.Id.t option ->
  ('model Entry.t, 'model Entry.Id.t option) Component.t Lwt.t

(** {2 Internal use} *)

val prepare :
  label: string ->
  search: (Slice.t -> string -> (int * 'model Entry.t list, string) Result.t Lwt.t) ->
  unserialise: ('model Entry.Id.t -> 'model Entry.t option Lwt.t) ->
  make_descr: ('model Entry.t -> string Lwt.t) ->
  make_result:
  (?classes: string list ->
  ?action: Utils.ResultRow.action ->
  ?prefix: Utils.ResultRow.cell list ->
  ?suffix: Utils.ResultRow.cell list ->
  'model Entry.t ->
  Utils.ResultRow.t) ->
  ?make_more_results:
  ('model Entry.t ->
  Utils.ResultRow.t list S.t) ->
  model_name: string ->
  create_dialog_content: (('model Entry.t, 'any) Editor.mode -> Page.t Lwt.t) ->
  unit ->
  ('model Entry.t, 'model Entry.Id.t option) Component.s
(** Variant of {!make} that only prepares the component. It must still be
    {!Component.initialise}d. This is used for composition with eg.
    {!ComponentList}. *)

val prepare_opt :
  label: string ->
  search: (Slice.t -> string -> (int * 'model Entry.t list, string) Result.t Lwt.t) ->
  unserialise: ('model Entry.Id.t -> 'model Entry.t option Lwt.t) ->
  make_descr: ('model Entry.t -> string Lwt.t) ->
  make_result:
  (?classes: string list ->
  ?action: Utils.ResultRow.action ->
  ?prefix: Utils.ResultRow.cell list ->
  ?suffix: Utils.ResultRow.cell list ->
  'model Entry.t ->
  Utils.ResultRow.t) ->
  ?make_more_results:
  ('model Entry.t ->
  Utils.ResultRow.t list S.t) ->
  model_name: string ->
  create_dialog_content: (('model Entry.t, 'any) Editor.mode -> Page.t Lwt.t) ->
  unit ->
  ('model Entry.t option, 'model Entry.Id.t option) Component.s
(** Variant of {!prepare} that is allowed to return no model. *)
