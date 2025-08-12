open Nes
open Common
open Model
open Html

let create ?context id =
  MainPage.madge_call_or_404 (Set Get) id @@ fun set ->
  Page.make'
    ~parent_title: "Set"
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_set id)
        (lwt @@ Any.set set);
    ]
    ~title: (lwt @@ Set.name' set)
    ~subtitles: [
      Formatters.Set.works' set;
      span
        [
          txt ((Kind.Dance.to_pretty_string % Set.kind') set);
          txt " — Play ";
          txt ((SetOrder.to_pretty_string % Set.order') set);
        ];
      (
        with_span_placeholder @@
          match%lwt Set.conceptors' set with
          | [] -> lwt_nil
          | devisers -> lwt [txt "Set by "; Formatters.Person.names' ~links: true devisers]
      );
    ]
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li [
                Utils.Button.make
                  ~label: "Share"
                  ~label_processing: "Sharing..."
                  ~icon: "share"
                  ~classes: ["dropdown-item"]
                  ~onclick: (fun () ->
                    Utils.write_to_clipboard @@ Utils.href_any_for_sharing (Set set);
                    Components.Toast.open_ ~title: "Copied to clipboard" [txt "The link to this set has been copied to your clipboard."];
                    lwt_unit
                  )
                  ();
              ];
              li
                [
                  a
                    ~a: [
                      a_class ["dropdown-item"];
                      a_href "#";
                      a_onclick (fun _ -> Lwt.async (fun () -> ignore <$> SetDownloadDialog.create_and_open set); false);
                    ]
                    [
                      i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                      txt " Download PDF";
                    ];
                ];
              li
                [
                  a
                    ~a: [
                      a_class ["dropdown-item"];
                      a_href Endpoints.Page.(href BookAdd);
                      a_onclick (fun _ -> BookEditor.add_to_storage id; true);
                    ]
                    [
                      i ~a: [a_class ["bi"; "bi-plus-square"]] [];
                      txt " Add to current book";
                    ];
                ];
            ];
        ];
      p
        [
          txt
            (
              match Set.instructions' set with
              | "" -> ""
              | instructions -> "Instructions: " ^ instructions
            )
        ];
      R.div
        (
          S.from'
            [
              div
                ~a: [a_class ["text-center"; "mt-4"]]
                [
                  h4 [span_placeholder ()];
                  div_placeholder ~min: 10 ~max: 20 ();
                ];
              div
                ~a: [a_class ["text-center"; "mt-4"]]
                [
                  h4 [span_placeholder ()];
                  div_placeholder ~min: 10 ~max: 20 ();
                ];
            ] @@
            let%lwt contents = Set.contents' set in
            Lwt_list.mapi_p
              (fun index (version, _parameters) ->
                let context = Endpoints.Page.inSet id index in
                (* FIXME: use parameters *)
                let%lwt tune = Version.tune' version in
                let id = Entry.id version in
                lwt @@
                  div
                    ~a: [a_class ["text-center"; "mt-4"]]
                    [
                      h4 [a ~a: [a_href (Endpoints.Page.href_version ~context id)] [txt @@ Tune.one_name' tune]];
                      Components.VersionSvg.make version;
                    ]
              )
              contents
        );
      Utils.quick_explorer_links'
        (lwt set)
        [
          ("books containing this set", Filter.(Any.book' % Book.memSet'));
        ];
    ]
