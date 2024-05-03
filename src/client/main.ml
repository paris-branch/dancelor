open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_html
open Dancelor_client_views
module Page = Dancelor_client_page

(* Whether to show the menu or not. [None] indicates the default (yes on
   desktop, no on mobile). *)
let (show_menu, set_show_menu) = React.S.create None

let header =
  header [
    div ~a:[a_class ["content"]] [

      (* Toggle for smartphone devices. *)
      a
        ~a:[
          a_id "to_nav";
          a_onclick (fun _ ->
              set_show_menu % Option.some % not @@ (S.value show_menu = Some true);
              false
            )
        ]
        [i ~a:[a_class ["material-symbols-outlined"]] [txt "menu"]];

      (* A glorious title. *)
      a ~a:[a_href "/"] [h1 [txt "Dancelor"]];

      (* Navigation menu. *)
      ul
        ~a:[
          a_id "nav";
          R.a_style (
            Fun.flip S.map show_menu @@ function
            | None -> ""
            | Some true -> "display: block;"
            | Some false -> "display: none;"
          )
        ]
        [
          li [
            a ~a:[a_href PageRouter.(path (Explore None))] [
              i ~a:[a_class ["material-symbols-outlined"]] [txt "search"];
              txt " Explore";
            ];
          ];

          li [
            txt "Add ";
            i ~a:[a_class ["material-symbols-outlined"]] [txt "arrow_drop_down"];

            ul ~a:[a_class ["subnav"]] [
              li [
                a ~a:[a_href PageRouter.(path PersonAdd)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "person"];
                  txt " Person";
                ]
              ];
              li [
                a ~a:[a_href PageRouter.(path DanceAdd)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "directions_walk"];
                  txt " Dance";
                ]
              ];
              li [
                a ~a:[a_href PageRouter.(path TuneAdd)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "music_note"];
                  txt " Tune";
                ];
              ];
              li [
                a ~a:[a_href PageRouter.(path VersionAdd)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "music_note"];
                  txt " Version";
                ];
              ];
              li [
                a ~a:[a_href PageRouter.(path SetCompose)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "format_list_bulleted"];
                  txt " Set";
                ];
              ];
              li [
                a ~a:[a_href PageRouter.(path BookCompose)] [
                  i ~a:[a_class ["material-symbols-outlined"]] [txt "library_books"];
                  txt " Book";
                ];
              ];
            ]
          ]
        ]
    ]
  ]

let dispatch url =
  let request = Madge_router.{ method_ = `GET ; path = Uri.path url ; query = Madge_query.from_uri url } in
  let page = Madge_router.request_to_resource request PageRouter.routes in
  match Option.get page with
  | PageRouter.Index -> Index.create ()
  | Explore query -> Explorer.create ?query ()
  | VersionAdd -> VersionEditor.create ()
  | Version {slug; context} -> VersionViewer.create slug ?context
  | TuneAdd -> TuneEditor.create ()
  | Tune {slug; context} -> TuneViewer.create slug ?context
  | SetCompose -> SetEditor.create ()
  | Set {slug; context} -> SetViewer.create slug ?context
  | BookCompose -> BookEditor.create ()
  | BookEdit slug -> BookEditor.create ~edit:slug ()
  | Book {slug; context} -> BookViewer.create slug ?context
  | PersonAdd -> PersonEditor.create ()
  | Person {slug; context} -> PersonViewer.create slug ?context
  | DanceAdd -> DanceEditor.create ()
  | Dance {slug; context} -> DanceViewer.create slug ?context

(** Used by {!on_load} to avoid garbage-collection of the iterators that store
    the state in the local storage. This is inspired by {!S.keep}, except
    {!S.keep} does not seem to work in our context and introduces a memory
    leak. *)
let gc_roots = ref []

let set_title title =
  Dom_html.document##.title := Js.string @@ match title with
    | "" -> "Dancelor"
    | title -> title ^ " | Dancelor"

let on_load _ev =
  let page = dispatch @@ Uri.of_string (Js.to_string Dom_html.window##.location##.href) in
  let iter_title = React.S.map set_title (Page.get_title page) in
  gc_roots := iter_title :: !gc_roots;
  Dom.appendChild Dom_html.document##.body (To_dom.of_header header);
  let content = To_dom.of_div (Page.get_content page) in
  content##.classList##add (Js.string "content");
  content##.classList##add (Js.string "page-body");
  Dom.appendChild Dom_html.document##.body content;
  Js._false

let _ =
  Dom_html.window##.onload := Dom_html.handler on_load
