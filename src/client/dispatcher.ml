open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_views

module Html = Dom_html

let js = Js.string

module type PAGE = sig
  type t
  val contents : t -> Html.divElement Js.t
  val init : t -> unit
  val refresh : t -> unit
end

let pack (type s) (module M : PAGE with type t = s) (create : Dancelor_client_elements.Page.t -> s) =
  (module struct
    type t = M.t
    let create = create
    let contents t = M.contents t
    let init t = M.init t
    let refresh t = M.refresh t
  end : Dancelor_client_elements.Page.CONTENTS)

let dispatch url =
  let request = Madge_router.{ method_ = `GET ; path = Uri.path url ; query = Madge_query.from_uri url } in
  let page = Madge_router.request_to_resource request PageRouter.routes in
  match Option.get page with
  | PageRouter.Index ->
    pack (module Index) Index.create
  | Explore query ->
    pack (module Explorer) (Explorer.create ?query)
  | VersionAdd ->
    pack (module VersionEditorInterface) VersionEditorInterface.create
  | Version {slug; context} ->
    pack (module VersionViewer) (VersionViewer.create slug ?context)
  | Tune {slug; context} ->
    pack (module TuneViewer) (TuneViewer.create slug ?context)
  | SetCompose ->
    pack (module SetEditorInterface) SetEditorInterface.create
  | Set {slug; context} ->
    pack (module SetViewer) (SetViewer.create slug ?context)
  | BookCompose ->
    pack (module BookEditorInterface) BookEditorInterface.create
  | BookEdit slug ->
    pack (module BookEditorInterface) (BookEditorInterface.update slug)
  | Book {slug; context} ->
    pack (module BookViewer) (BookViewer.create slug ?context)
  | PersonAdd ->
    pack (module PersonEditorInterface) (fun page -> PersonEditorInterface.create page)
  | Person {slug; context} ->
    pack (module PersonViewer) (PersonViewer.create slug ?context)
  | Dance {slug; context} ->
    pack (module DanceViewer) (DanceViewer.create slug ?context)
