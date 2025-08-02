open Nes
open Common

open Model
open Html

let create ?context id =
  MainPage.get_model_or_404 (Version Get) id @@ fun version ->
  let%lwt tune = Model.Version.tune' version in
  let other_versions_lwt =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Version Search)
        Slice.everything
        Formula.(
          and_l
            [
              Filter.Version.tuneIs' tune;
              not (Filter.Version.is' version);
            ]
        )
  in
  Page.make'
    ~parent_title: "Version"
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_version id)
        (lwt @@ Any.version version);
    ]
    ~title: (Version.one_name' version)
    ~subtitles: [
      Formatters.Version.tune_aka' version;
      Formatters.Version.tune_description' version;
      Formatters.Version.description' ~arranger_links: true version;
    ]
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li
                [
                  a
                    ~a: [
                      a_class ["dropdown-item"];
                      a_href "#";
                      a_onclick (fun _ -> Lwt.async (fun () -> ignore <$> VersionDownloadDialog.create_and_open version); false);
                    ]
                    [
                      i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                      txt " Download PDF";
                    ];
                ];
              li
                [
                  a
                    ~a: [a_class ["dropdown-item"]; a_href (Endpoints.Api.(href @@ Version Ly) id (Tune.slug' tune))]
                    [
                      i ~a: [a_class ["bi"; "bi-file-music"]] [];
                      txt " Download LilyPond";
                    ];
                ];
              li
                [
                  Components.Button.make
                    ~label: "Add to current set"
                    ~icon: "plus-square"
                    ~classes: ["dropdown-item"]
                    ~onclick: (fun _ ->
                      SetEditor.Editor.add_to_storage id;
                      Components.Toast.open_
                        ~title: "Added to current set"
                        [
                          txt "This version was added to the current set. You can see that in the ";
                          a ~a: [a_href Endpoints.Page.(href SetAdd)] [txt "set editor"];
                          txt ".";
                        ];
                      lwt_unit
                    )
                    ()
                ];
              R.li
                (
                  S.from' [] @@
                    match%lwt Tune.scddb_id' <$> Model.Version.tune' version with
                    | None -> lwt_nil
                    | Some scddb_id ->
                      lwt
                        [
                          a
                            ~a: [
                              a_class ["dropdown-item"];
                              a_href (Uri.to_string @@ SCDDB.tune_uri scddb_id);
                            ]
                            [
                              i ~a: [a_class ["bi"; "bi-box-arrow-up-right"]] [];
                              txt " See on SCDDB";
                            ]
                        ]
                );
            ];
        ];
      div
        [
          with_span_placeholder @@
            match%lwt Tune.date' <$> Model.Version.tune' version with
            | None -> lwt_nil
            | Some date ->
              lwt [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        ];
      div ~a: [a_class ["text-center"]] [Components.VersionSvg.make version];
      div
        ~a: [a_class ["d-flex"; "justify-content-end"]]
        [
          audio
            ~a: [a_controls ()]
            ~src: (Endpoints.Api.(href @@ Version Ogg) id (Tune.slug' tune) Model.VersionParameters.none RenderingParameters.none)
            []
        ];
      Utils.quick_explorer_links
        [
          ("sets containing this version", lwt @@ Filter.(Any.set' % Set.memVersion') version);
          ("sets containing this tune", Filter.(Any.set' % Set.existsVersion' % Version.tuneIs') <$> Model.Version.tune' version);
          ("books containing this version", lwt @@ Filter.(Any.book' % Book.memVersionDeep') version);
          ("books containing this tune", Filter.(Any.book' % Book.existsVersionDeep' % Version.tuneIs') <$> Model.Version.tune' version);
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Other Versions"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt other_versions = other_versions_lwt in
                let%lwt tune_href = (Endpoints.Page.href_tune % Entry.id) <$> Model.Version.tune' version in
                lwt
                  (
                    if other_versions = [] then
                        [p [txt "There are no other versions available for this tune."]]
                    else
                      [
                        Tables.versions other_versions;
                        p
                          [
                            txt "You can also go to the ";
                            a ~a: [a_href tune_href] [txt "page of the tune"];
                            txt "."
                          ]
                      ]
                  )
            );
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances That Recommend This Tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt dances = Tune.dances' =<< Model.Version.tune' version in
                lwt
                  [
                    if dances = [] then
                      txt "There are no dances that recommend this tune."
                    else
                      Tables.dances dances
                  ]
            )
        ];
    ]
