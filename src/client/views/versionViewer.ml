open Nes
open Common

open Model
open Html

let create ?context slug =
  MainPage.get_model_or_404 (Version Get) slug @@ fun version ->
  let other_versions_lwt =
    let%lwt tune = Model.Version.tune' version in
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
  Lwt.return @@
    Page.make
      ~parent_title: "Version"
      ~title: (S.from' "" @@ Version.name' version)
      ~before_title: [
        Components.ContextLinks.make_and_render
          ?context
          ~this_page: (Endpoints.Page.href_version slug)
          (Lwt.return @@ Any.version version);
      ]
      [
        h5 ~a: [a_class ["text-center"]] [Formatters.Version.tune_aka' version];
        h5 ~a: [a_class ["text-center"]] [Formatters.Version.tune_description' version];
        h5 ~a: [a_class ["text-center"]] [Formatters.Version.description' ~arranger_links: true version];
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
                R.li
                  (
                    S.from' [] @@
                      match%lwt Lwt.map Tune.scddb_id' (Model.Version.tune' version) with
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
        div
          [
            with_span_placeholder @@
              match%lwt Lwt.map Tune.date' (Model.Version.tune' version) with
              | None -> Lwt.return_nil
              | Some date ->
                Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
          ];
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
            ("sets containing this version", Lwt.return @@ Filter.(Any.set' % Set.memVersion') version);
            ("sets containing this tune", Lwt.map Filter.(Any.set' % Set.existsVersion' % Version.tuneIs') (Model.Version.tune' version));
            ("books containing this version", Lwt.return @@ Filter.(Any.book' % Book.memVersionDeep') version);
            ("books containing this tune", Lwt.map Filter.(Any.book' % Book.existsVersionDeep' % Version.tuneIs') (Model.Version.tune' version));
          ];
        div
          ~a: [a_class ["section"]]
          [
            h3 [txt "Other Versions"];
            R.div
              (
                S.from' (Tables.placeholder ()) @@
                  let%lwt other_versions = other_versions_lwt in
                  let%lwt tune_href = Lwt.map (Endpoints.Page.href_tune % Entry.slug) (Model.Version.tune' version) in
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
                  let%lwt dances = Lwt.bind (Model.Version.tune' version) Tune.dances' in
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
