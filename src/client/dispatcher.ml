open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_views
module Page = Dancelor_client_page

(** Used by {!pack} to avoid garbage-collection of the iterators that
    store the state in the local storage. This is inspired by {!S.keep}, except
    {!S.keep} does not seem to work in our context and introduces a memory
    leak. *)
let gc_roots = ref []

(* FIXME: Compatibility layer with the old pages *)
let pack (create : unit -> Page.new_api) =
  let open Js_of_ocaml_tyxml.Tyxml_js in
  (module struct
    type t = Dom_html.divElement Js.t

    let init _ = ()
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
    pack Index.create
  | Explore query ->
    pack @@ Explorer.create ?query
  | VersionAdd ->
    pack VersionEditor.create
  | Version {slug; context} ->
    pack @@ fun () -> VersionViewer.create slug ?context
  | TuneAdd ->
    pack TuneEditor.create
  | Tune {slug; context} ->
    pack @@ fun () -> TuneViewer.create slug ?context
  | SetCompose ->
    pack SetEditor.create
  | Set {slug; context} ->
    pack @@ fun () -> SetViewer.create slug ?context
  | BookCompose ->
    pack BookEditor.create
  | BookEdit slug ->
    pack @@ BookEditor.create ~edit:slug
  | Book {slug; context} ->
    pack @@ fun () -> BookViewer.create slug ?context
  | PersonAdd ->
    pack PersonEditor.create
  | Person {slug; context} ->
    pack @@ fun () -> PersonViewer.create slug ?context
  | DanceAdd ->
    pack DanceEditor.create
  | Dance {slug; context} ->
    pack @@ fun () -> DanceViewer.create slug ?context
