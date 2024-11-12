open Nes
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components
module Page = Dancelor_client_page
module Utils = Dancelor_client_utils
open Dancelor_client_html

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
  Page.make ~title: (Page.sub_title "Version" title) @@
    div
      [
        download_dialog_button;
        ly_download_button;
        add_to_current_set_button;
      ]
    );

    L.div (
      match%lwt Lwt.map Tune.date tune_lwt with
      | None -> Lwt.return_nil
      | Some date ->
        Lwt.return [txt "Composed "; txt (PartialDate.to_pretty_string ~at:true date); txt "."]
    );
    L.div (
      match%lwt Lwt.map Tune.scddb_id tune_lwt with
      | None -> Lwt.return_nil
      | Some scddb_id ->
        let href = Uri.to_string @@ SCDDB.tune_uri scddb_id in
        Lwt.return [
          txt "See on ";
          a ~a:[a_href href; a_target "blank"] [
            txt "the Strathspey Database"
          ];
          txt ".";
        ]
    );

    div ~a:[a_class ["after-buttons"]] [];

    div ~a:[a_class ["section"]] [
      div ~a:[a_class ["image-container"]] [
        object_ ~a:[
          a_mime_type "image/svg+xml";
          a_data ApiRouter.(path_versionSvg slug)
        ] [];
      ]
    ];

    div ~a:[a_class ["audio-container"]] [
      audio ~a:[a_controls ()]
        ~src:ApiRouter.(path_versionOgg slug)
        []
    ];

    Utils.quick_explorer_links [
      ("sets containing this version", Lwt.map (Any.Filter.set' % Set.Filter.memVersion') version_lwt);
      ("sets containing this tune", Lwt.map (Any.Filter.set' % Set.Filter.existsVersion' % Version.Filter.tuneIs') tune_lwt);
      ("books containing this version", Lwt.map (Any.Filter.book' % Book.Filter.memVersionDeep') version_lwt);
      ("books containing this tune", Lwt.map (Any.Filter.book' % Book.Filter.existsVersionDeep' % Version.Filter.tuneIs') tune_lwt);
    ];

    div ~a:[a_class ["section"]] [
      h3 [txt "Other Versions"];

      L.div (
        let%lwt other_versions = other_versions_lwt in

        Lwt.return (
          if other_versions = [] then
            [p [txt "There are no other versions available for this tune."]]
          else
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
                    a_data ApiRouter.(path_versionSvg slug)
                  ]
                  [];
              ]
          ];
        div
          ~a: [a_class ["audio-container"]]
          [
            audio
              ~a: [a_controls ()]
              ~src: ApiRouter.(path_versionOgg slug)
              []
          ];
        L.div
          ~a: [a_class ["buttons"]]
          (
            let%lwt is_broken = Lwt.map Version.broken version_lwt in
            Lwt.return
              [
                a
                  ~a: [
                    a_class ["button"; "button-danger"];
                    a_onclick
                      (fun _ ->
                        Lwt.async
                          (fun () ->
                            if is_broken then version_lwt >>=| Version.mark_fixed else version_lwt >>=| Version.mark_broken;%lwt
                            Dom_html.window##.location##reload;
                            Lwt.return_unit
                          );
                        false
                      )
                  ]
                  [
                    txt (if is_broken then "Mark fixed" else "Mark broken");
                  ]
              ]
          );
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
                        Dancelor_client_tables.versions other_versions;
                        p
                          [
                            txt "You can also go to the ";
                            a
                              ~a: [L.a_href @@ Lwt.map (PageRouter.path_tune % Tune.slug) tune_lwt]
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
                      Dancelor_client_tables.dances dances
                  ]
              )
          ];
      ]
