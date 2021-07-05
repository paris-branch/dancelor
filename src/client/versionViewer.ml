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
  let other_versions_lwt =
    let%lwt tune = tune_lwt in
    let%lwt version = version_lwt in
    let filter =
      Formula.(and_l [
          Version.Filter.tuneIs tune;
          not_ (Version.Filter.is version);
        ])
    in
    Version.all ~filter ()
  in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (tune_lwt >>=| Tune.name) ];
      h3 ~classes:["title"] [ text_lwt (Formatters.Tune.aka_lwt tune_lwt) ];
      h3 ~classes:["title"] [ text_lwt (Formatters.Tune.recommended_lwt tune_lwt) ];
      h3_lwt ~classes:["title"] (
        let%lwt tune = tune_lwt in
        let%lwt descr = Formatters.Tune.description tune page in
        Lwt.return (List.map node_of_dom_node descr)
      );
      h3_lwt ~classes:["title"] (
        let%lwt version = version_lwt in
        let%lwt descr = Formatters.Version.description version page in
        Lwt.return (List.map node_of_dom_node descr)
      );

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

        let source =
          spf "/%s%s"
            Constant.api_prefix
            (Router.path_of_controller (Router.VersionSvg slug) |> snd)
          |> Lwt.return
        in
        let img = Image.create ~source page in
        node_of_dom_node (Image.root img :> dom_node)
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
                node_of_dom_node
                  (Table.root (Dancelor_client_tables.Version.make other_versions_lwt page)
                   :> dom_node);

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
        h3 [ text "Sets in Which This Version Appears" ];

        div_lwt (
          let sets_lwt =
            let%lwt version = version_lwt in
            let filter = Set.Filter.memVersion version in
            Set.all ~filter ()
          in
          let%lwt sets = sets_lwt in

          Lwt.return [
            if sets = [] then
              text "There are no sets containing this version."
            else
              node_of_dom_node
                (Table.root (Dancelor_client_tables.Set.make sets_lwt page)
                 :> dom_node)
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
          let books_lwt =
            let%lwt version = version_lwt in
            let filter = Book.Filter.memVersionDeep version in
            Book.all ~filter ()
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              text "There are no books containing this version."
            else
              node_of_dom_node
                (Table.root (Dancelor_client_tables.Book.make books_lwt page)
                 :> dom_node)
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
