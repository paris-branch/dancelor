open Nes
open Common

open Model
open Html

let create ?context slug =
  let version_lwt = MainPage.get_model_or_404 (Version Get) slug in
  let tune_lwt = version_lwt >>=| Version.tune' in
  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    Lwt.map snd @@
      Madge_client.call_exn
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
  let title = S.from' "" (Lwt.map Tune.name' tune_lwt) in
  Page.make
    ~parent_title: "Version"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_version slug)
        (Lwt.map Any.version version_lwt);
    ]
    [
      L.h5 ~a: [a_class ["text-center"]] (Lwt.map Formatters.Tune.aka' tune_lwt);
      L.h5 ~a: [a_class ["text-center"]] (tune_lwt >>=| Formatters.Tune.description');
      L.h5 ~a: [a_class ["text-center"]] (version_lwt >>=| Formatters.Version.description' ~arranger_links: true);
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
                      a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (VersionDownloadDialog.create_and_open slug)); false);
                    ]
                    [
                      i ~a: [a_class ["bi"; "bi-file-pdf"]] [];
                      txt " Download PDF";
                    ];
                ];
              li
                [
                  a
                    ~a: [a_class ["dropdown-item"]; a_href (Endpoints.Api.(href @@ Version Ly) slug)]
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
                      SetEditor.Editor.add_to_storage slug;
                      Components.Toast.open_
                        ~title: "Added to current set"
                        [
                          txt "This version was added to the current set. You can see that in the ";
                          a ~a: [a_href Endpoints.Page.(href SetAdd)] [txt "set editor"];
                          txt ".";
                        ];
                      Lwt.return ()
                    )
                    ()
                ];
              L.li
                (
                  match%lwt Lwt.map Tune.scddb_id' tune_lwt with
                  | None -> Lwt.return_nil
                  | Some scddb_id ->
                    Lwt.return
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
      L.div
        (
          match%lwt Lwt.map Tune.date' tune_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div ~a: [a_class ["text-center"]] [Components.VersionSvg.make slug];
      div
        ~a: [a_class ["d-flex"; "justify-content-end"]]
        [
          audio
            ~a: [a_controls ()]
            ~src: (Endpoints.Api.(href @@ Version Ogg) slug Model.VersionParameters.none RenderingParameters.none)
            []
        ];
      Utils.quick_explorer_links
        [
          ("sets containing this version", Lwt.map Filter.(Any.set' % Set.memVersion') version_lwt);
          ("sets containing this tune", Lwt.map Filter.(Any.set' % Set.existsVersion' % Version.tuneIs') tune_lwt);
          ("books containing this version", Lwt.map Filter.(Any.book' % Book.memVersionDeep') version_lwt);
          ("books containing this tune", Lwt.map Filter.(Any.book' % Book.existsVersionDeep' % Version.tuneIs') tune_lwt);
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Other Versions"];
          L.div
            (
              let%lwt other_versions = other_versions_lwt in
              Lwt.return
                (
                  if other_versions = [] then
                      [p [txt "There are no other versions available for this tune."]]
                  else
                    [
                      Tables.versions other_versions;
                      p
                        [
                          txt "You can also go to the ";
                          a
                            ~a: [L.a_href @@ Lwt.map (Endpoints.Page.href_tune % Entry.slug) tune_lwt]
                            [txt "page of the tune"];
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
          L.div
            (
              let%lwt tune = tune_lwt in
              let%lwt dances = Tune.dances' tune in
              Lwt.return
                [
                  if dances = [] then
                    txt "There are no dances that recommend this tune."
                  else
                    Tables.dances dances
                ]
            )
        ];
    ]
