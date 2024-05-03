open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters
module Components = Dancelor_client_components
module Page = Dancelor_client_page
open Dancelor_client_html

let create ?context slug =
  let version_lwt = Version.get slug in
  let tune_lwt = version_lwt >>=| Version.tune in
  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    Version.search' Formula.(and_l [
        Version.Filter.tuneIs' tune;
        not (Version.Filter.is' version);
      ])
  in
  let title = S.from' "" (Lwt.map Tune.name tune_lwt) in
  Page.make_new_api ~title:(Page.sub_title "Version" title) @@
  div [
    Components.ContextLinks.make_and_render
      ?context
      ~this_page: (PageRouter.path_version slug)
      (Lwt.map Any.version version_lwt);

    h2 ~a:[a_class ["title"]] [R.txt title];
    L.h3 ~a:[a_class ["title"]] (Lwt.map Formatters.Tune.aka tune_lwt);
    L.h3 ~a:[a_class ["title"]] (tune_lwt >>=| Formatters.Tune.description);
    L.h3 ~a:[a_class ["title"]] (version_lwt >>=| Formatters.Version.description ~link:true);

    L.div (
      match%lwt Lwt.map Tune.date tune_lwt with
      | None -> Lwt.return_nil
      | Some date ->
        Lwt.return [txt "Date: "; txt (PartialDate.to_pretty_string date)]
    );
    L.div (
      match%lwt Lwt.map Tune.scddb_id tune_lwt with
      | None -> Lwt.return_nil
      | Some scddb_id ->
        let href = Uri.to_string @@ SCDDB.tune_uri scddb_id in
        Lwt.return [
          h3 ~a:[a_class ["title"]] [
            a ~a:[a_href href; a_target "blank"] [
              txt "Link to the Strathspey Database"
            ]
          ]
        ]
    );

    div ~a:[a_class ["buttons"]] (

      let download_dialog_button =
        a
          ~a:[
            a_class ["button"];
            a_onclick (fun _ -> Lwt.async (fun () -> Lwt.map ignore (VersionDownloadDialog.create_and_open slug)); false);
          ]
          [
            i ~a:[a_class ["material-symbols-outlined"]] [txt "picture_as_pdf"];
            txt " PDF";
          ]
      in

      let ly_download_button =
        a
          ~a:[
            a_class ["button"];
            a_href ApiRouter.(path_versionLy slug);
          ]
          [
            i ~a:[a_class ["material-symbols-outlined"]] [txt "article"];
            txt " LilyPond"
          ]
      in

      let add_to_current_set_button =
        a
          ~a:[
            a_class ["button"];
            a_href PageRouter.(path SetCompose);
            a_onclick (fun _ -> SetEditor.Editor.add_to_storage slug; true);
          ]
          [
            i ~a:[a_class ["material-symbols-outlined"]] [txt "add_box"];
            txt " Add to current set";
          ]
      in

      [
        download_dialog_button;
        ly_download_button;
        add_to_current_set_button;
      ]
    );

    div ~a:[a_class ["section"]] [
      h3 [txt "Previsualisation"];

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

    L.div ~a:[a_class ["buttons"]] (
      let%lwt is_broken = Lwt.map Version.broken version_lwt in

      Lwt.return [
        a ~a:[
          a_class ["button"; "button-danger"];
          a_onclick
            (fun _ ->
               Lwt.async
                 (fun () ->
                    if is_broken then version_lwt >>=| Version.mark_fixed else version_lwt >>=| Version.mark_broken;%lwt
                    Dom_html.window##.location##reload ;
                    Lwt.return_unit);
               false)
        ] [
          txt (if is_broken then "Mark fixed" else "Mark broken");
        ]
      ]
    );

    div ~a:[a_class ["section"]] [
      h3 [txt "Other Versions"];

      L.div (
        let%lwt other_versions = other_versions_lwt in

        Lwt.return (
          if other_versions = [] then
            [p [txt "There are no other versions available for this tune."]]
          else
            [
              Dancelor_client_tables.versions other_versions;

              p [
                txt "You can also go to the ";
                a ~a:[L.a_href @@ Lwt.map (PageRouter.path_tune % Tune.slug) tune_lwt]
                  [txt "page of the tune"];
                txt "."
              ]
            ]
        )
      );
    ];

    div ~a:[a_class ["section"]] [
      h3 [txt "Dances That Recommend This Tune"];

      L.div (
        let%lwt tune = tune_lwt in
        let%lwt dances = Tune.dances tune in

        Lwt.return [
          if dances = [] then
            txt "There are no dances that recommend this tune."
          else
            Dancelor_client_tables.dances dances
        ]
      )
    ];

    div ~a:[a_class ["section"]] [
      h3 [txt "Sets in Which This Version Appears"];

      L.div (
        let%lwt sets =
          let%lwt version = version_lwt in
          Set.search' @@ Set.Filter.memVersion' version
        in

        Lwt.return [
          if sets = [] then
            txt "There are no sets containing this version."
          else
            Dancelor_client_tables.sets sets
        ]
      );

      L.div (
        Fun.flip Lwt.map other_versions_lwt @@ function
        | [] -> []
        | _ -> [
            p [
              txt "If you want to see the sets in which this version or ";
              txt "any other appear, go to the ";
              a ~a:[L.a_href @@ Lwt.map (PageRouter.path_tune % Tune.slug) tune_lwt]
                [txt "page of the tune"];
              txt "."
            ]
          ]
      )
    ];

    div ~a:[a_class ["section"]] [
      h3 [txt "Books in Which This Version Appears"];

      L.div (
        let%lwt books =
          let%lwt version = version_lwt in
          Book.search' @@ Book.Filter.memVersionDeep' version
        in

        Lwt.return [
          if books = [] then
            txt "There are no books containing this version."
          else
            Dancelor_client_tables.books books
        ]
      );

      L.div (
        Fun.flip Lwt.map other_versions_lwt @@ function
        | [] -> []
        | _ -> [
            p [
              txt "If you want to see the books in which this version or ";
              txt "any other appear, go to the ";
              a ~a:[L.a_href @@ Lwt.map (PageRouter.path_tune % Tune.slug) tune_lwt]
                [txt "page of the tune"];
              txt "."
            ]
          ]
      );
    ];
  ]
