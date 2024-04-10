open Js_of_ocaml_tyxml.Tyxml_js
open Dancelor_client_model
open Dancelor_client_html

type 'model t

val make :
  search: (Slice.t -> string -> (int * 'result list, string) Result.t Lwt.t) ->
  ('result list -> ('result list, string) Result.t) ->
  'result t

val signal : 'result t -> ('result list, string) result S.t

val clear : 'result t -> unit

val render :
  make_result: (
    ?onclick: (unit -> unit) ->
    ?prefix: Html_types.td Html.elt list ->
    ?suffix: Html_types.td Html.elt list ->
    'result ->
    Html_types.tr Html.elt
  ) ->
  'result t ->
  [> Html_types.div ] Html.elt
