open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_views
module Page = Dancelor_client_page

let js = Js.string

module type PAGE = sig
  type t
  val contents : t -> Dom_html.divElement Js.t
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
  | Explore query ->
    pack (module Explorer) (Explorer.create ?query)
  | VersionAdd ->
    pack (module VersionEditor) VersionEditor.create
  | Version {slug; context} ->
    pack (module VersionViewer) (VersionViewer.create slug ?context)
  | TuneAdd ->
    pack (module TuneEditor) TuneEditor.create
  | Tune {slug; context} ->
    pack (module TuneViewer) (TuneViewer.create slug ?context)
  | SetCompose ->
    pack (module SetEditor) SetEditor.create
  | Set {slug; context} ->
    pack (module SetViewer) (SetViewer.create slug ?context)
  | BookCompose ->
    pack (module BookEditor) BookEditor.create
  | BookEdit slug ->
    pack (module BookEditor) (BookEditor.create ~edit:slug)
  | Book {slug; context} ->
    pack (module BookViewer) (BookViewer.create slug ?context)
  | PersonAdd ->
    pack (module PersonEditor) PersonEditor.create
  | Person {slug; context} ->
    pack (module PersonViewer) (PersonViewer.create slug ?context)
  | DanceAdd ->
    pack (module DanceEditor) DanceEditor.create
  | Dance {slug; context} ->
    pack (module DanceViewer) (DanceViewer.create slug ?context)
