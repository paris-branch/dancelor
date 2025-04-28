open Nes
open Common

open Js_of_ocaml
open Html

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
      Lwt.map Result.ok @@
        Madge_client.call_exn Endpoints.Api.(route @@ Any Search) slice filter
    )
    ()

let quick_search_to_explorer value =
  let href = Endpoints.Page.(href Explore) (Some value) in
  Dom_html.window##.location##.href := Js.string href;
  Js_of_ocaml_lwt.Lwt_js.sleep 10.

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
                            List.map
                              (fun (icon, key, text) ->
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
                              )
                              [
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
                          List.map
                            (fun (icon, href, text) ->
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
                            )
                            [
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
                  Auth.header_item;
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

let initial_content = Dom_html.(createDiv document)
let current_content = ref None

let initialise () =
  assert (!current_content = None);
  current_content := Some initial_content;
  Dom.appendChild Dom_html.document##.body (To_dom.of_header header);
  add_slash_quick_search_event_listener ();
  Dom.appendChild Dom_html.document##.body initial_content;
  Dom.appendChild Dom_html.document##.body (To_dom.of_footer footer)

let load page =
  assert (!current_content <> None);
  let iter_title = React.S.map set_title (Page.full_title page) in
  Depart.keep_forever iter_title;
  let (page_on_load, page_content) = Page.render page in
  let page_content = To_dom.of_div page_content in
  Dom.replaceChild Dom_html.document##.body page_content (Option.get !current_content);
  current_content := Some page_content;
  page_on_load ()

exception ReplacementSuccessful
exception ReplacementFailed

(** Variant of {!load}, that, after loading, sleeps for a bit, then raises
    either {!ReplacementFailed} or {!ReplacementSuccessful} dependending on the
    status of things. It is intended to be used in an asynchronous promise that
    loses meaning once the page replacement has taken place. The async exception
    hook should ignore {!ReplacementSuccessful} and report
    {!ReplacementFailed}. *)
let load_sleep_raise ?(delay = 1.) page =
  let previous_content = !current_content in
  load page;
  Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
  if !current_content = previous_content then
    Lwt.fail ReplacementFailed
  else
    Lwt.fail ReplacementSuccessful

let get_model_or_404 endpoint slug =
  match%lwt Madge_client.call Endpoints.Api.(route @@ endpoint) slug with
  | Ok model -> Lwt.return model
  | Error Madge_client.{status; _} -> load_sleep_raise (OooopsViewer.create status)
