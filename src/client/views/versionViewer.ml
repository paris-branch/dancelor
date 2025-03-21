open Nes
open Common

open Model
open Html

let create ?context slug =
  let version_lwt = Version.get slug in
  let tune_lwt = version_lwt >>=| Version.tune in
  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    Version.search'
      Formula.(
        and_l
          [
            Version.Filter.tuneIs' tune;
            not (Version.Filter.is' version);
          ]
      )
  in
  let title = S.from' "" (Lwt.map Tune.name tune_lwt) in
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
      L.h3 ~a: [a_class ["title"]] (Lwt.map Formatters.Tune.aka tune_lwt);
      L.h3 ~a: [a_class ["title"]] (tune_lwt >>=| Formatters.Tune.description);
      L.h3 ~a: [a_class ["title"]] (version_lwt >>=| Formatters.Version.description ~link: true);
      div
        ~a: [a_class ["buttons"]]
        (
          let download_dialog_button =
            a
              ~a: [
                a_class ["button"];
                a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (VersionDownloadDialog.create_and_open slug)); false);
              ]
              [
                i ~a: [a_class ["material-symbols-outlined"]] [txt "picture_as_pdf"];
                txt " PDF";
              ]
          in
          let ly_download_button =
            a
              ~a: [
                a_class ["button"];
                a_href (Endpoints.Api.(href @@ Version Ly) slug);
              ]
              [
                i ~a: [a_class ["material-symbols-outlined"]] [txt "article"];
                txt " LilyPond"
              ]
          in
          let add_to_current_set_button =
            a
              ~a: [
                a_class ["button"];
                a_href Endpoints.Page.(href SetAdd);
                a_onclick (fun _ -> SetEditor.Editor.add_to_storage slug; true);
              ]
              [
                i ~a: [a_class ["material-symbols-outlined"]] [txt "add_box"];
                txt " Add to current set";
              ]
          in
          [
            download_dialog_button;
            ly_download_button;
            add_to_current_set_button;
          ]
        );
      L.div
        (
          match%lwt Lwt.map Tune.date tune_lwt with
          | None -> Lwt.return_nil
          | Some date ->
            Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      L.div
        (
          match%lwt Lwt.map Tune.scddb_id tune_lwt with
          | None -> Lwt.return_nil
          | Some scddb_id ->
            let href = Uri.to_string @@ SCDDB.tune_uri scddb_id in
            Lwt.return
              [
                txt "See on ";
                a
                  ~a: [a_href href; a_target "blank"]
                  [
                    txt "the Strathspey Database"
                  ];
                txt ".";
              ]
        );
      div ~a: [a_class ["after-buttons"]] [];
      div
        ~a: [a_class ["section"]]
        [
          div
            ~a: [a_class ["image-container"]]
            [
              object_
                ~a: [
                  a_mime_type "image/svg+xml";
                  a_data (Endpoints.Api.(href @@ Version Svg) Model.VersionParameters.none slug)
                ]
                [];
            ]
        ];
      div
        ~a: [a_class ["audio-container"]]
        [
          audio
            ~a: [a_controls ()]
            ~src: (Endpoints.Api.(href @@ Version Ogg) Model.VersionParameters.none slug)
            []
        ];
      Utils.quick_explorer_links
        [
          ("sets containing this version", Lwt.map (Any.Filter.set' % Set.Filter.memVersion') version_lwt);
          ("sets containing this tune", Lwt.map (Any.Filter.set' % Set.Filter.existsVersion' % Version.Filter.tuneIs') tune_lwt);
          ("books containing this version", Lwt.map (Any.Filter.book' % Book.Filter.memVersionDeep') version_lwt);
          ("books containing this tune", Lwt.map (Any.Filter.book' % Book.Filter.existsVersionDeep' % Version.Filter.tuneIs') tune_lwt);
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
              let%lwt dances = Tune.dances tune in
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
