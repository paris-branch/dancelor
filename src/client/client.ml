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

let a_data_bs_toggle = a_user_data "bs-toggle"
let a_data_bs_target = a_user_data "bs-target"

let a_aria_controls = a_aria "controls" % List.singleton
let a_aria_expanded = a_aria "expanded" % List.singleton % Bool.to_string
let a_aria_label = a_aria "label" % List.singleton

let header =
  nav
    ~a: [a_class ["navbar"; "navbar-expand-sm"; "navbar-dark"; "bg-primary"; "mb-2"]]
    [
      div
        ~a: [a_class ["container"]]
        [
          a
            ~a: [a_class ["navbar-brand"; "my-n2"; "py-0"]; a_href "/"]
            [
              img ~a: [a_height 60] ~src: "/logo.svg" ~alt: "Dancelor" ();
            ];
          button
            ~a: [a_class ["navbar-toggler"]; a_button_type `Button; a_data_bs_toggle "collapse"; a_data_bs_target "#the-navigation"; a_aria_controls "the-navigation"; a_aria_expanded false; a_aria_label "Toggle navigation"]
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
                        ~a: [a_button_type `Button; a_class ["btn"; "btn-primary"; "dropdown-toggle"]; a_data_bs_toggle "dropdown"; a_aria_expanded false]
                        [
                          txt "Explore"
                        ];
                      ul
                        ~a: [a_class ["dropdown-menu"]]
                        (
                          [
                            li [a ~a: [a_class ["dropdown-item"]; a_href (Endpoints.Page.(href Explore) None)] [txt "All"]];
                            li [hr ~a: [a_class ["dropdown-divider"]] ()];
                          ] @
                          List.map
                            (fun (key, text) ->
                               let href = Endpoints.Page.(href Explore) @@ Option.some @@ TextFormula.(to_string (Formula.pred (Unary ("type", Formula.pred (Raw key))))) in
                               li [a ~a: [a_class ["dropdown-item"]; a_href href] [txt text]]
                            )
                            [
                              ("person", "Persons");
                              ("dance", "Dances");
                              ("tune", "Tunes");
                              ("version", "Versions");
                              ("set", "Sets");
                              ("book", "Books");
                            ]
                        );
                    ];
                  li
                    ~a: [a_class ["nav-item"; "dropdown"]]
                    [
                      button
                        ~a: [a_button_type `Button; a_class ["btn"; "btn-primary"; "dropdown-toggle"]; a_data_bs_toggle "dropdown"; a_aria_expanded false]
                        [
                          txt "Add"
                        ];
                      ul
                        ~a: [a_class ["dropdown-menu"]]
                        (
                          let open Endpoints.Page in
                          List.map
                            (fun (href, text) ->
                               li [a ~a: [a_class ["dropdown-item"]; a_href href] [txt text]]
                            )
                            [
                              (href PersonAdd, "Person");
                              (href DanceAdd, "Dance");
                              (href TuneAdd, "Tune");
                              (href VersionAdd None, "Version");
                              (href SetAdd, "Set");
                              (href BookAdd, "Book");
                            ]
                        );
                    ];
                ];
            ];
          button
            ~a: [a_button_type `Button; a_class ["btn"; "btn-light"; "ms-2"; "d-none"; "d-sm-block"]]
            [
              i ~a: [a_class ["bi"; "bi-search"]] [];
              txt " Search ";
              span ~a: [a_class ["badge"; "text-bg-secondary"]] [txt "/"];
            ];
        ];
    ]

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
  Dom.appendChild Dom_html.document##.body (To_dom.of_div @@ Page.render page);
  Dom.appendChild Dom_html.document##.body (To_dom.of_footer footer);
  Js._false

let _ =
  Dom_html.window##.onload := Dom_html.handler on_load
