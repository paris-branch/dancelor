open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let version_lwt = Version.get slug in
  let tune_lwt = version_lwt >>=| Version.tune in

  Lwt.async (fun () ->
      let%lwt tune = tune_lwt in
      let%lwt name = Tune.name tune in
      document##.title := js (name ^ " | Tune | Dancelor");
      Lwt.return ()
    );

  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    let filter =
      Formula.(and_l [
          VersionFilter.tuneIs tune;
          not_ (VersionFilter.is version);
        ])
    in
    Version.search filter
    >|=| Score.list_erase
  in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (tune_lwt >>=| Tune.name) ];
      h3_lwt ~classes:["title"] (tune_lwt >>=| Formatters.Tune.aka);
      h3_lwt ~classes:["title"] (tune_lwt >>=| Formatters.Tune.description);
      h3_lwt ~classes:["title"] (version_lwt >>=| Formatters.Version.description ~link:true);

      div ~classes:["buttons"] (
        let pdf_href, ly_href =
          Helpers.build_path ~api:true ~route:(Router.VersionPdf slug) (),
          Helpers.build_path ~api:true ~route:(Router.VersionLy slug) ()
        in

        let pdf, ly =
          Inputs.Button.create ~href:(Lwt.return pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
          Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
        in

        [
          node_of_dom_node (Inputs.Button.root pdf :> dom_node);
          node_of_dom_node (Inputs.Button.root ly  :> dom_node);
        ]
      );

      div ~classes:["section"] [
        h3 [ text "Previsualisation" ];

        div ~classes:["image-container"] [
          let src_lwt =
            spf "/%s%s"
              Constant.api_prefix
              (Router.path_of_controller (Router.VersionSvg slug) |> snd)
            |> Lwt.return
          in
          img ~src_lwt ();
        ]
      ];

      div ~classes:["audio-container"] [
        let src_lwt =
          spf "/%s%s"
            Constant.api_prefix
            (Router.path_of_controller (Router.VersionOgg slug) |> snd)
          |> Lwt.return
        in
        audio ~src_lwt ~controls:true ()
      ];

      div ~classes:["section"] [
        h3 [ text "Other Versions" ];

        div_lwt (
          let%lwt other_versions = other_versions_lwt in

          Lwt.return (
            if other_versions = [] then
              [ p [ text "There are no other versions available for this tune." ] ]
            else
              [
                Dancelor_client_tables.versions other_versions;

                let href_lwt =
                  let%lwt tune = tune_lwt in
                  let%lwt slug = Tune.slug tune in
                  Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
                in
                p [
                  text "You can also go to the ";
                  a ~href_lwt [ text "page of the tune" ];
                  text "."
                ]
              ]
          )
        );
      ];

      div ~classes:["section"] [
        h3 [ text "Dances That Recommend This Tune" ];

        div_lwt (
          let none = (Page.document page)##createTextNode (js "") in
          let none_maybe = Dom_html.createP (Page.document page) in
          Dom.appendChild none_maybe none;
          Dom.appendChild content none_maybe;

          let%lwt tune = tune_lwt in
          let%lwt dances = Tune.dances tune in

          Lwt.return [
            if dances = [] then
              text "There are no dances that recommende this tune."
            else
              Dancelor_client_tables.dances dances
          ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Sets in Which This Version Appears" ];

        div_lwt (
          let sets_lwt =
            let%lwt version = version_lwt in
            let filter = SetFilter.memVersion version in
            Set.search filter
            >|=| Score.list_erase
          in
          let%lwt sets = sets_lwt in

          Lwt.return [
            if sets = [] then
              text "There are no sets containing this version."
            else
              Dancelor_client_tables.sets sets
          ]
        );

        div_lwt (
          match%lwt other_versions_lwt with
          | [] -> Lwt.return_nil
          | _ -> Lwt.return [
              let href_lwt =
                let%lwt tune = tune_lwt in
                let%lwt slug = Tune.slug tune in
                Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
              in
              p [
                text "If you want to see the sets in which this version or ";
                text "any other appear, go to the ";
                a ~href_lwt [ text "page of the tune" ];
                text "."
              ]
            ]
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Books in Which This Version Appears" ];

        div_lwt (
          let%lwt books =
            let%lwt version = version_lwt in
            let filter = BookFilter.memVersionDeep version in
            Book.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if books = [] then
              text "There are no books containing this version."
            else
              Dancelor_client_tables.books books
          ]
        );

        div_lwt (
          match%lwt other_versions_lwt with
          | [] -> Lwt.return_nil
          | _ -> Lwt.return [
              let href_lwt =
                let%lwt tune = tune_lwt in
                let%lwt slug = Tune.slug tune in
                Lwt.return (Router.path_of_controller (Router.Tune slug) |> snd)
              in
              p [
                text "If you want to see the books in which this version or ";
                text "any other appear, go to the ";
                a ~href_lwt [ text "page of the tune" ];
                text "."
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
