open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_views

module Html = Dom_html

let js = Js.string

module type PAGE = sig
  type t
  val contents : t -> Html.divElement Js.t
  val init : t -> unit
  val refresh : t -> unit
end

let pack (type s) (module M : PAGE with type t = s) (create : Page.t -> s) =
  (module struct
    type t = M.t
    let create = create
    let contents t = M.contents t
    let init t = M.init t
    let refresh t = M.refresh t
  end : Page.CONTENTS)

let dispatch url =
  let request = Madge_router.{ method_ = `GET ; path = Uri.path url ; query = Madge_query.from_uri url } in
  let page = Madge_router.request_to_resource request PageRouter.routes in
  match Option.get page with
  | PageRouter.Index ->
    pack (module Index) Index.create
  | Search q ->
    pack (module Search) (Search.create q)
  | VersionAdd ->
    pack (module VersionEditorInterface) VersionEditorInterface.create
  | VersionAll ->
    pack (module VersionExplorer) VersionExplorer.create
  | VersionBroken ->
    pack (module VersionBrokenExplorer) VersionBrokenExplorer.create
  | Version slug ->
    pack (module VersionViewer) (VersionViewer.create slug)
  | Tune slug ->
    pack (module TuneViewer) (TuneViewer.create slug)
  | SetAll ->
    pack (module SetExplorer) SetExplorer.create
  | SetCompose ->
    pack (module SetEditorInterface) SetEditorInterface.create
  | Set slug ->
    pack (module SetViewer) (SetViewer.create slug)
  | BookAll ->
    pack (module BookExplorer) BookExplorer.create
  | BookCompose ->
    pack (module BookEditorInterface) BookEditorInterface.create
  | BookEdit slug ->
    pack (module BookEditorInterface) (BookEditorInterface.update slug)
  | Book slug ->
    pack (module BookViewer) (BookViewer.create slug)
  | CreditAdd ->
    pack (module CreditEditorInterface) (fun page -> CreditEditorInterface.create page)
  | Credit slug ->
    pack (module CreditViewer) (CreditViewer.create slug)
  | Person slug ->
    pack (module PersonViewer) (PersonViewer.create slug)
  | Dance slug ->
    pack (module DanceViewer) (DanceViewer.create slug)
