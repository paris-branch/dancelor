open Nes
open Common

open Js_of_ocaml
open Html
open Views

let set_title title =
  Dom_html.document##.title :=
    Js.string @@
      match title with
      | "" -> "Dancelor"
      | title -> title ^ " | Dancelor"

let get_uri () = Uri.of_string (Js.to_string Dom_html.window##.location##.href)

let quick_search =
  Components.Search.Quick.make
    ~search: (fun slice input ->
      let%rlwt filter = Lwt.return (Model.Any.Filter.from_string input) in
      Lwt.map Result.ok @@ Model.Any.search slice filter
    )
    ()

let quick_search_to_explorer value =
  let href = Endpoints.Page.(href Explore) (Some value) in
  Dom_html.window##.location##.href := Js.string href;
  Lwt.pmsleep 10.

let open_quick_search () =
  Page.open_dialog ~hide_body_overflow_y: true @@ fun return ->
  Components.Search.Quick.render
    ~return
    ~dialog_title: (S.const "Quick search")
    ~dialog_buttons: [
      Components.Button.make
        ~label: "Explore"
        ~label_processing: "Opening explorer..."
        ~icon: "zoom-in"
        ~badge: "↵"
        ~classes: ["btn-primary"]
        ~onclick: (fun () -> quick_search_to_explorer (S.value @@ Components.Search.Quick.text quick_search))
        ();
    ]
    ~on_enter: (fun value -> Lwt.async (fun () -> quick_search_to_explorer value))
    ~make_result: (fun ~context result -> Utils.AnyResult.make_result ~context result)
    quick_search

let header =
  nav
    ~a: [a_class ["navbar"; "navbar-expand-sm"; "navbar-dark"; "bg-primary"; "mb-2"]]
    [
      div
        ~a: [a_class ["container"]]
        [
          a
            ~a: [a_class ["navbar-brand"; "my-n2"; "py-0"; "flex-fill"]; a_href "/"]
            [
              img ~a: [a_height 60] ~src: "/logo.svg" ~alt: "Dancelor" ();
            ];
          Components.Button.make
            ~icon: "search"
            ~classes: ["btn-light"; "me-2"; "d-block"; "d-sm-none"]
            ~onclick: (Lwt.map ignore % open_quick_search)
            ();
          button
            ~a: [a_class ["navbar-toggler"]; a_button_type `Button; a_user_data "bs-toggle" "collapse"; a_user_data "bs-target" "#the-navigation"; a_aria "controls" ["the-navigation"]; a_aria "expanded" ["false"]; a_aria "label" ["Toggle navigation"]]
            [
              span ~a: [a_class ["navbar-toggler-icon"]] [];
            ];
          div
            ~a: [a_class ["collapse"; "navbar-collapse"]; a_id "the-navigation"]
            [
              ul
                ~a: [a_class ["navbar-nav"; "ms-auto"]]
                [
                  li
                    ~a: [a_class ["nav-item"; "dropdown"]]
                    [
                      button
                        ~a: [a_button_type `Button; a_class ["btn"; "btn-primary"; "dropdown-toggle"]; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
                        [
                          txt "Explore"
                        ];
                      ul
                        ~a: [a_class ["dropdown-menu"]]
                        (
                          [li [a ~a: [a_class ["dropdown-item"]; a_href (Endpoints.Page.(href Explore) None)] [txt "All"]];
                          li [hr ~a: [a_class ["dropdown-divider"]] ()];
                          ] @
                            List.map (fun (icon, key, text) ->
                                let href = Endpoints.Page.(href Explore) @@ Option.some @@ TextFormula.(to_string (Formula.pred (Unary ("type", Formula.pred (Raw key))))) in
                                li
                                  [
                                    a
                                      ~a: [a_class ["dropdown-item"]; a_href href]
                                      [
                                        i ~a: [a_class ["bi"; "bi-" ^ icon]] [];
                                        txt " ";
                                        txt text
                                      ]
                                  ]
                              )[
                                ("archive", "source", "Sources");
                                ("person", "person", "Persons");
                                ("person-arms-up", "dance", "Dances");
                                ("music-note-list", "tune", "Tunes");
                                ("music-note-beamed", "version", "Versions");
                                ("list-stars", "set", "Sets");
                                ("book", "book", "Books");
                              ]
                        );
                    ];
                  li
                    ~a: [a_class ["nav-item"; "dropdown"]]
                    [
                      button
                        ~a: [a_button_type `Button; a_class ["btn"; "btn-primary"; "dropdown-toggle"]; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
                        [
                          txt "Add"
                        ];
                      ul
                        ~a: [a_class ["dropdown-menu"]]
                        (
                          let open Endpoints.Page in
                          List.map (fun (icon, href, text) ->
                              li
                                [
                                  a
                                    ~a: [a_class ["dropdown-item"]; a_href href]
                                    [
                                      i ~a: [a_class ["bi"; "bi-" ^ icon]] [];
                                      txt " ";
                                      txt text
                                    ]
                                ]
                            )[
                              ("archive", href SourceAdd, "Source");
                              ("person", href PersonAdd, "Person");
                              ("person-arms-up", href DanceAdd, "Dance");
                              ("music-note-list", href TuneAdd, "Tune");
                              ("music-note-beamed", href VersionAdd None, "Version");
                              ("list-stars", href SetAdd, "Set");
                              ("book", href BookAdd, "Book");
                            ]
                        );
                    ];
                ];
            ];
          Components.Button.make
            ~label: "Search"
            ~icon: "search"
            ~badge: "/"
            ~classes: ["btn-light"; "ms-2"; "d-none"; "d-sm-block"]
            ~onclick: (Lwt.map ignore % open_quick_search)
            ();
        ];
      ]

    (* Add an event listener to open the quick search by pressing '/'. *)
    let add_slash_quick_search_event_listener () =
      Utils.add_target_event_listener
        Dom_html.window
        Dom_html.Event.keydown
        (fun event target ->
          if not (Utils.is_input target) && event##.keyCode = 191 then (* slash *)
            (Lwt.async (Lwt.map ignore % open_quick_search); Js._false)
          else
            Js._true
        )

    let footer =
      nav
        ~a: [a_class ["navbar"; "navbar-expand-sm"; "navbar-dark"; "bg-primary"; "mt-4"]]
        [
          div
            ~a: [a_class ["container"; "d-flex"; "flex-column"; "flex-sm-row"]]
            [
              a ~a: [a_class ["text-light"; "my-1"]; a_href "/"] [txt "Dancelor"];
              a
                ~a: [
                  a_class ["icon-link"; "text-light"; "my-1"];
                  a_href "https://github.com/paris-branch/dancelor";
                  a_target "_blank";
                ]
                [
                  i ~a: [a_class ["bi"; "bi-github"]] [];
                  txt "paris-branch/dancelor";
                ];
              Components.Button.make
                ~label: "Report an issue"
                ~label_processing: "Reporting..."
                ~icon: "bug"
                ~classes: ["btn-light"; "my-1"]
                ~onclick: (fun () ->
                  Lwt.map ignore @@ IssueReport.open_dialog @@ get_uri ()
                )
                ()
            ];
        ]

    let dispatch uri =
      let dispatch : type a r. (a, Page.t, r) Endpoints.Page.t -> a = function
        | Index -> Index.create ()
        | Explore -> (fun query -> Explorer.create ?query ())
        | Book -> (fun context slug -> BookViewer.create ?context slug)
        | BookAdd -> BookEditor.create ()
        | BookEdit -> (fun slug -> BookEditor.create ~edit: slug ())
        | Dance -> (fun context slug -> DanceViewer.create ?context slug)
        | DanceAdd -> DanceEditor.create ()
        | Person -> (fun context slug -> PersonViewer.create ?context slug)
        | PersonAdd -> PersonEditor.create ()
        | Version -> (fun context slug -> VersionViewer.create ?context slug)
        | VersionAdd -> (fun tune -> VersionEditor.create ~tune: (Option.to_list tune) ())
        | Tune -> (fun context slug -> TuneViewer.create ?context slug)
        | TuneAdd -> TuneEditor.create ()
        | Set -> (fun context slug -> SetViewer.create ?context slug)
        | SetAdd -> SetEditor.create ()
        | Source -> (fun context slug -> SourceViewer.create ?context slug)
        | SourceAdd -> SourceEditor.create ()
      in
      let madge_match_apply_all : Page.t Endpoints.Page.wrapped' list -> (unit -> Page.t) option =
        List.map_first_some @@ fun (Endpoints.Page.W endpoint) ->
        Madge.match_' (Endpoints.Page.route endpoint) (dispatch endpoint) {meth = GET; uri; body = ""}
      in
      match madge_match_apply_all Endpoints.Page.all_endpoints' with
      | Some page -> page ()
      | None -> (* FIXME: 404 page *) assert false

    let on_load _ev =
      let page = dispatch @@ get_uri () in
      let iter_title = React.S.map set_title (Page.full_title page) in
      Depart.keep_forever iter_title;
      Dom.appendChild Dom_html.document##.body (To_dom.of_header header);
      add_slash_quick_search_event_listener ();
      Dom.appendChild Dom_html.document##.body (To_dom.of_div @@ Page.render page);
      Dom.appendChild Dom_html.document##.body (To_dom.of_footer footer);
      Js._false

    let _ =
      Dom_html.window##.onload := Dom_html.handler on_load
