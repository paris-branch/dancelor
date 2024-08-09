open Nes
open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_model
open Dancelor_client_html
module Utils = Dancelor_client_utils

type 'model t

val make :
  ?has_interacted: bool S.t ->
  search: (Slice.t -> string -> (int * 'model list, string) Result.t Lwt.t) ->
  serialise: ('model -> 'model Slug.t) ->
  unserialise: ('model Slug.t -> 'model Lwt.t) ->
  'model Slug.t option ->
  'model t

val raw_signal : 'model t -> 'model Slug.t option S.t

val signal : 'model t -> ('model, string) Result.t S.t

val clear : 'model t -> unit

val render :
  make_result: (
    ?classes: string list ->
    ?action: Utils.ResultRow.action ->
    ?prefix: Utils.ResultRow.cell list ->
    ?suffix: Utils.ResultRow.cell list ->
    'model ->
    Utils.ResultRow.t
  ) ->
  field_name: (string * string) ->
  model_name: string ->
  create_dialog_content: (
    ?on_save:('model -> unit) ->
    string ->
    Html_types.div Html.elt
  ) ->
  'model t ->
  [> Html_types.div ] Html.elt
