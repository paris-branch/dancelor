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

let quick_search_to_explorer value =
  let href = Endpoints.Page.(href Explore) (Some value) in
  Dom_html.window##.location##.href := Js.string href;
  Js_of_ocaml_lwt.Lwt_js.sleep 10.

let quick_search =
  Components.Search.Quick.make
    ~search: (fun slice input ->
      let%rlwt filter = lwt (Filter.Any.from_string input) in
      ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Any Search) slice filter
    )
    ~on_enter: (fun value -> Lwt.async (fun () -> quick_search_to_explorer value))
    ()

let open_quick_search () =
  Page.open_dialog ~hide_body_overflow_y: true @@ fun return ->
  Components.Search.Quick.render
    ~return
    ~dialog_title: (lwt "Quick search")
    ~dialog_buttons: [
      Utils.Button.make
        ~label: "Explore"
        ~label_processing: "Opening explorer..."
        ~icon: "zoom-in"
        ~badge: "↵"
        ~classes: ["btn-primary"]
        ~onclick: (fun () -> quick_search_to_explorer (S.value @@ Components.Search.Quick.text quick_search))
        ();
    ]
    ~make_result: (fun ~context result -> Utils.AnyResult.make_result ~context result)
    quick_search

let nav_item_explore =
  li
    ~a: [a_class ["nav-item"; "dropdown"]]
    [
      Utils.Button.make
        ~label: "Explore"
        ~classes: ["text-white"; "dropdown-toggle"]
        ~more_a: [a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
        ();
      ul
        ~a: [a_class ["dropdown-menu"]]
        (
          [li [Utils.Button.make_a ~label: "All" ~href: (S.const @@ Endpoints.Page.(href Explore) None) ~dropdown: true ()];
          li [hr ~a: [a_class ["dropdown-divider"]] ()];
          ] @
            List.map
              (fun (icon, key, label) ->
                let href = S.const @@ Endpoints.Page.(href Explore) @@ some @@ TextFormula.(to_string (Formula.pred (Unary ("type", Formula.pred (Raw key))))) in
                li [Utils.Button.make_a ~label ~icon ~href ~dropdown: true ()]
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
    ]

let nav_item_create =
  if%lwt Permission.can_create () then
    lwt [
      li
        ~a: [a_class ["nav-item"; "dropdown"]]
        [
          Utils.Button.make
            ~label: "Add"
            ~classes: ["text-white"; "dropdown-toggle"]
            ~more_a: [a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]]
            ();
          ul
            ~a: [a_class ["dropdown-menu"]]
            (
              let open Endpoints.Page in
              List.map
                (fun (icon, href, label) ->
                  li [Utils.Button.make_a ~label ~icon ~href: (S.const href) ~dropdown: true ()]
                )
                [
                  ("archive", href SourceAdd, "Source");
                  ("person", href PersonAdd, "Person");
                  ("person-arms-up", href DanceAdd, "Dance");
                  ("music-note-list", href TuneAdd, "Tune");
                  ("music-note-beamed", href VersionAdd, "Version");
                  ("list-stars", href SetAdd, "Set");
                  ("book", href BookAdd, "Book");
                ]
            );
        ]
    ]
  else
    lwt_nil

let header =
  nav
    ~a: [
      R.a_class (
        S.l2
          (@)
          (S.map (function Environment.Offline -> ["bg-secondary"] | _ -> ["bg-primary"]) Environment.run_status)
          (S.const ["navbar"; "navbar-expand-sm"; "navbar-dark"; "mb-2"])
      )
    ]
    [
      div
        ~a: [a_class ["container-md"]]
        [
          a
            ~a: [a_class ["navbar-brand"; "my-n2"; "py-0"; "flex-fill"]; a_href "/"]
            [
              img ~a: [a_height 60] ~src: "/logo.svg" ~alt: "Dancelor" ();
            ];
          Utils.Button.make
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
              R.ul
                ~a: [a_class ["navbar-nav"; "ms-auto"]]
                (
                  S.map List.flatten @@
                  S.all @@ [
                    S.const [nav_item_explore];
                    S.from' [] nav_item_create;
                    S.const [UserHeader.header_item];
                  ]
                );
            ];
          Utils.Button.make
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
    ~a: [
      R.a_class (
        S.l2
          (@)
          (S.map (function Environment.Offline -> ["bg-secondary"] | _ -> ["bg-primary"]) Environment.run_status)
          (S.const ["navbar"; "navbar-expand-sm"; "navbar-dark"; "mt-4"])
      )
    ]
    [
      div
        ~a: [a_class ["container-md"; "d-flex"; "flex-column"; "flex-sm-row"]]
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
          Utils.Button.make
            ~label: "Report an issue"
            ~label_processing: "Reporting..."
            ~icon: "bug"
            ~classes: ["btn-light"; "my-1"]
            ~onclick: (fun () ->
              ignore <$> IssueReport.open_dialog @@ get_uri ()
            )
            ()
        ];
    ]

let never_returns () =
  Js_of_ocaml_lwt.Lwt_js.sleep Float.infinity;%lwt
  assert false

let initial_content =
  To_dom.of_div @@
  snd @@
  Page.render @@
  Page.make
    ~title: (never_returns ())
    ~subtitles: (
      List.init 2 (fun _ ->
        with_span_placeholder @@ never_returns ()
      )
    )
    (
      List.init 10 (fun _ ->
        with_span_placeholder ~min: 8 ~max: 12 @@
          never_returns ()
      )
    )

let current_content = ref None

let initialise () =
  assert (!current_content = None);
  current_content := Some initial_content;
  Dom.appendChild Dom_html.document##.body (To_dom.of_header header);
  add_slash_quick_search_event_listener ();
  Dom.appendChild Dom_html.document##.body initial_content;
  Dom.appendChild Dom_html.document##.body (To_dom.of_footer footer)

let load page_promise =
  assert (!current_content <> None);
  (* FIXME: Loading Dancelor logo until page promise resolves *)
  let%lwt page = page_promise in
  let iter_title = React.S.map set_title (Page.full_title page) in
  Depart.keep_forever iter_title;
  let (page_on_load, page_content) = Page.render page in
  let page_content = To_dom.of_div page_content in
  Dom.replaceChild Dom_html.document##.body page_content (Option.get !current_content);
  current_content := Some page_content;
  page_on_load ();
  lwt_unit

exception ReplacementSuccessful
exception ReplacementFailed

(** Variant of {!load}, that, after loading, sleeps for a bit, then raises
    either {!ReplacementFailed} or {!ReplacementSuccessful} dependending on the
    status of things. It is intended to be used in an asynchronous promise that
    loses meaning once the page replacement has taken place. The async exception
    hook should ignore {!ReplacementSuccessful} and report
    {!ReplacementFailed}. *)
let load_sleep_raise ?(delay = 1.) page_promise =
  let previous_content = !current_content in
  load page_promise;%lwt
  Js_of_ocaml_lwt.Lwt_js.sleep delay;%lwt
  if !current_content = previous_content then
    Lwt.fail ReplacementFailed
  else
    Lwt.fail ReplacementSuccessful

let madge_call_or_404 endpoint arg f =
  try%lwt
    f =<< Madge_client.call_exn Endpoints.Api.(route @@ endpoint) arg
  with
    | Madge_client.(Error (Http {status; _})) -> OooopsViewer.create status

let assert_can_create f =
  match%lwt Permission.can_create () with
  | true -> f ()
  | false -> OooopsViewer.create `Forbidden

let assert_can_admin f =
  match%lwt Permission.can_admin () with
  | true -> f ()
  | false -> OooopsViewer.create `Forbidden
