open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_html
open Dancelor_client_views
module Components = Dancelor_client_components
module Page = Dancelor_client_page
module Utils = Dancelor_client_utils
module Model = Dancelor_client_model

(* FIXME: This search is duplicated in so many places. *)
let search slice input =
  let threshold = 0.4 in
  let%rlwt filter = Lwt.return (Model.Any.Filter.from_string input) in (* FIXME: AnyFilter.from_string should return a result lwt *)
  let%lwt results = Model.Any.search ~threshold ~slice filter in
  Lwt.return_ok results

let set_title title =
  Dom_html.document##.title :=
    Js.string @@
      match title with
      | "" -> "Dancelor"
      | title -> title ^ " | Dancelor"

let get_page () =
  let url = Uri.of_string (Js.to_string Dom_html.window##.location##.href) in
  let request = Madge_router.{method_ = `GET; path = Uri.path url; query = Madge_query.from_uri url} in
  Madge_router.request_to_resource request PageRouter.routes

(* Whether to show the menu or not. [None] indicates the default (yes on
   desktop, no on mobile). *)
let (show_menu, set_show_menu) = React.S.create None

let path_explore_models m =
  Model.(
    PageRouter.path @@
      PageRouter.Explore
        (
          Some
            (
              TextFormula.(to_string (Formula.pred (Unary ("type", Formula.pred (Raw m)))))
            )
        )
  )

let header =
  header
    [
      div
        ~a: [a_class ["content"]]
        [

          (* Toggle for smartphone devices. *)
          a
            ~a: [
              a_id "to_nav";
              a_onclick (fun _ ->
                set_show_menu % Option.some % not @@ (S.value show_menu = Some true);
                false
              )
            ]
            [i ~a: [a_class ["material-symbols-outlined"]] [txt "menu"]];

          (* A glorious title. *)
          a
            ~a: [
              a_href "/";
              a_class ["logo"];
            ]
            [
              img
                ~src: "/logo.svg"
                ~alt: "Dancelor"
                ()
            ];

          (* Navigation menu. *)
          ul
            ~a: [
              a_id "nav";
              R.a_style
                (
                  Fun.flip S.map show_menu @@ function
                    | None -> ""
                    | Some true -> "display: block;"
                    | Some false -> "display: none;"
                )
            ]
            [
              li
                [
                  Components.QuickSearchBar.make_and_render
                    ~placeholder: "Quick search (press '/')"
                    ~search
                    ~make_result: (fun ?classes any -> Utils.AnyResult.make_result ?classes any)
                    ~on_enter: (fun search_text ->
                      Dom_html.window##.location##.href := Js.string PageRouter.(path_explore (Some search_text))
                    )
                    ~focus_on_slash: true
                    ()
                ];
              li
                [
                  a
                    ~a: [a_href PageRouter.(path (Explore None))]
                    [
                      txt "Explore";
                      i ~a: [a_class ["material-symbols-outlined"]] [txt "arrow_drop_down"];
                    ];
                  ul
                    ~a: [a_class ["subnav"]]
                    [
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "person")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "person"];
                              txt " Persons";
                            ]
                        ];
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "dance")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "directions_walk"];
                              txt " Dances";
                            ]
                        ];
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "tune")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "music_note"];
                              txt " Tunes";
                            ]
                        ];
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "version")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "music_note"];
                              txt " Versions";
                            ];
                        ];
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "set")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "format_list_bulleted"];
                              txt " Sets";
                            ];
                        ];
                      li
                        [
                          a
                            ~a: [a_href (path_explore_models "book")]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "library_books"];
                              txt " Books";
                            ];
                        ];
                    ];
                ];
              li
                [
                  txt "Add";
                  i ~a: [a_class ["material-symbols-outlined"]] [txt "arrow_drop_down"];
                  ul
                    ~a: [a_class ["subnav"]]
                    [
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path PersonAdd)]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "person"];
                              txt " Person";
                            ]
                        ];
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path DanceAdd)]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "directions_walk"];
                              txt " Dance";
                            ]
                        ];
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path TuneAdd)]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "music_note"];
                              txt " Tune";
                            ];
                        ];
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path_versionAdd ())]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "music_note"];
                              txt " Version";
                            ];
                        ];
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path SetCompose)]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "format_list_bulleted"];
                              txt " Set";
                            ];
                        ];
                      li
                        [
                          a
                            ~a: [a_href PageRouter.(path BookCompose)]
                            [
                              i ~a: [a_class ["material-symbols-outlined"]] [txt "library_books"];
                              txt " Book";
                            ];
                        ];
                    ]
                ]
            ]
        ]
    ]

let issue_report_button =
  div
    ~a: [a_id "issue-report-button"]
    [
      a
        ~a: [
          a_onclick (fun _ ->
            Lwt.async (fun () ->
              Lwt.map ignore @@
              IssueReportDialog.open_ @@
              get_page ()
            );
            false
          );
        ]
        [
          i ~a: [a_class ["material-symbols-outlined"]] [txt "bug_report"];
          span [txt " Report an issue"];
        ];
    ]

let dispatch page =
  (* FIXME: When option is none, redirect to a 404 page. *)
  match Option.get page with
  | PageRouter.Index -> Index.create ()
  | Explore query -> Explorer.create ?query ()
  | VersionAdd {tune} -> VersionEditor.create ~tune: (Option.to_list tune) ()
  | Version {slug; context} -> VersionViewer.create slug ?context
  | TuneAdd -> TuneEditor.create ()
  | Tune {slug; context} -> TuneViewer.create slug ?context
  | SetCompose -> SetEditor.create ()
  | Set {slug; context} -> SetViewer.create slug ?context
  | BookCompose -> BookEditor.create ()
  | BookEdit slug -> BookEditor.create ~edit: slug ()
  | Book {slug; context} -> BookViewer.create slug ?context
  | PersonAdd -> PersonEditor.create ()
  | Person {slug; context} -> PersonViewer.create slug ?context
  | DanceAdd -> DanceEditor.create ()
  | Dance {slug; context} -> DanceViewer.create slug ?context

let on_load _ev =
  let page = dispatch @@ get_page () in
  let iter_title = React.S.map set_title (Page.get_title page) in
  Depart.keep_forever iter_title;
  Dom.appendChild Dom_html.document##.body (To_dom.of_header header);
  let content = To_dom.of_div (Page.get_content page) in
  content##.classList##add (Js.string "content");
  content##.classList##add (Js.string "page-body");
  Dom.appendChild Dom_html.document##.body content;
  Dom.appendChild Dom_html.document##.body (To_dom.of_div issue_report_button);
  Js._false

let _ =
  Dom_html.window##.onload := Dom_html.handler on_load
