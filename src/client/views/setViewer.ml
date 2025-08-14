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
    ~share: (Set set)
    ~actions: (
      lwt
        [
          Utils.Button.make_a
            ~classes: ["dropdown-item"]
            ~href: (S.const @@ Endpoints.Page.(href SetEdit) id)
            ~icon: "pencil-square"
            ~label: "Edit"
            ();
          Utils.Button.make
            ~classes: ["dropdown-item"]
            ~onclick: (fun _ -> ignore <$> SetDownloadDialog.create_and_open set)
            ~icon: "file-pdf"
            ~label: "Download PDF"
            ();
          Utils.Button.make
            ~classes: ["dropdown-item"]
            ~onclick: (fun _ ->
              BookEditor.add_set_to_storage id;%lwt
              Utils.Toast.open_
                ~title: "Added to current book"
                [
                  txt "This set was added to the current book. You can see that in the ";
                  a ~a: [a_href Endpoints.Page.(href BookAdd)] [txt "book editor"];
                  txt ".";
                ];
              lwt_unit
            )
            ~icon: "plus-square"
            ~label: "Add to current book"
            ()
        ]
    )
    [
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
