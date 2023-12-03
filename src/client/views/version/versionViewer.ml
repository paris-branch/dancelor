open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let version_lwt = Version.get slug in
  let tune_lwt = version_lwt >>=| Version.tune in

  Lwt.on_success tune_lwt (fun tune ->
      document##.title := js (Tune.name tune ^ " | Tune | Dancelor");
    );

  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    let filter =
      Formula.(and_l [
          Version.Filter.tuneIs tune;
          not_ (Version.Filter.is version);
        ])
    in
    Version.search' filter >|=| Score.list_erase
  in

  let open Dancelor_client_html in

  let (download_dialog, show_download_dialog) = VersionDownloadDialog.create_and_render slug in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [L.txt @@ Lwt.map Tune.name tune_lwt];
      L.h3 ~a:[a_class ["title"]] (Lwt.map Formatters.Tune.aka tune_lwt);
      L.h3 ~a:[a_class ["title"]] (tune_lwt >>=| Formatters.Tune.description);
      L.h3 ~a:[a_class ["title"]] (version_lwt >>=| Formatters.Version.description ~link:true);
      L.div (
        match%lwt Lwt.map Tune.scddb_id tune_lwt with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.tune_uri scddb_id in
          Lwt.return [
            h3 ~a:[a_class ["title"]] [
              a ~a:[a_href href; a_target "blank"] [
                txt "Link to the Strathspey Database"
              ]
            ]
          ]
      );

      download_dialog;

      div ~a:[a_class ["buttons"]] (

        let download_dialog_button =
          a
            ~a:[
              a_class ["button"];
              a_onclick (fun _ -> show_download_dialog (); false);
            ]
            [
              i ~a:[a_class ["fas"; "fa-file-pdf"]] [];
              txt " PDF";
            ]
        in

        let ly_download_button =
          a
            ~a:[
              a_class ["button"];
              a_href ApiRouter.(path @@ versionLy slug);
            ]
            [
              i ~a:[a_class ["fas"; "fa-file-alt"]] [];
              txt " LilyPond"
            ]
        in

        let add_to_current_set_button =
          button
            ~a:[
              a_button_type `Button;
              a_onclick
                (fun _ ->
                   SetEditor.add_to_storage slug;
                   Dom_html.window##.location##.href := js PageRouter.(path SetCompose);
                   false
                );
            ]
            [
              txt "Add to current set"
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
            a_data ApiRouter.(path (versionSvg slug None))
          ] [];
        ]
      ];

      div ~a:[a_class ["audio-container"]] [
        audio ~a:[a_controls ()]
          ~src:ApiRouter.(path (versionOgg slug))
          []
      ];

      L.div ~a:[a_class ["buttons"]] (
        let%lwt is_broken = Lwt.map Version.broken version_lwt in

        Lwt.return [
          button ~a:[
            a_button_type `Button;
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
          let none = (Dancelor_client_elements.Page.document page)##createTextNode (js "") in
          let none_maybe = Dom_html.createP (Dancelor_client_elements.Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

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
          let sets_lwt =
            let%lwt version = version_lwt in
            let filter = Set.Filter.memVersion version in
            Set.search' filter >|=| Score.list_erase
          in
          let%lwt sets = sets_lwt in

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
            let filter = Book.Filter.memVersionDeep version in
            Book.search' filter >|=| Score.list_erase
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
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
