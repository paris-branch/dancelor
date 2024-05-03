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

(** Used by {!packNewAPI} to avoid garbage-collection of the iterators that
    store the state in the local storage. This is inspired by {!S.keep}, except
    {!S.keep} does not seem to work in our context and introduces a memory
    leak. *)
let gc_roots = ref []

(* FIXME: Compatibility layer with the old pages *)
let packNewAPI (create : unit -> Page.new_api) =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  (module struct
    type t = Dom_html.divElement Js.t

    let init _ = ()
    let refresh _ = ()
    let contents = Fun.id

    let create _ =
      let document = Dom_html.document in
      let contents = Dom_html.createDiv document in
      let new_api = create () in
      let iter_title = React.S.map (fun title -> document##.title := Js.string (title ^ " | Dancelor")) new_api.title in
      gc_roots := iter_title :: !gc_roots;
      Dom.appendChild contents (To_dom.of_div new_api.content);
      contents
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
    packNewAPI DanceEditor.create
  | Dance {slug; context} ->
    pack (module DanceViewer) (DanceViewer.create slug ?context)
