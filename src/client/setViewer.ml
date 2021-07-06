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
  let set_lwt = Set.get slug in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (set_lwt >>=| Set.name) ];
      h3_lwt ~classes:["title"] (set_lwt >>=| Formatters.Set.works);
      h3 ~classes:["title"] [ text_lwt (let open Lwt in set_lwt >>=| Set.kind >|= Kind.dance_to_pretty_string) ];
      h3_lwt ~classes:["title"] (
        match%lwt set_lwt >>=| Set.deviser with
        | None -> Lwt.return_nil
        | Some deviser ->
          let%lwt line_block = Formatters.Credit.line (Some deviser) in
          Lwt.return (text "Set devised by " :: line_block)
      );

      div ~classes:["buttons"] (
        let bass_parameters =
          SetParameters.(
            make ~every_version:VersionParameters.(
                make
                  ~clef:Music.Bass
                  ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1)))
                  ()
              )
              ()
          )
        in
        let b_parameters = SetParameters.make_instrument (Music.make_pitch B Flat (-1)) in
        let e_parameters = SetParameters.make_instrument (Music.make_pitch E Flat 0) in

        let c_pdf_href, b_pdf_href, e_pdf_href, bass_pdf_href,
            ly_href
          =
          Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
            (),
          Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
            ~query:["parameters", [
                b_parameters
                |> SetParameters.to_yojson |> Yojson.Safe.to_string
              ]] (),
          Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
            ~query:["parameters", [
                e_parameters
                |> SetParameters.to_yojson |> Yojson.Safe.to_string
              ]] (),
          Helpers.build_path ~api:true ~route:(Router.SetPdf slug)
            ~query:["parameters", [
                bass_parameters
                |> SetParameters.to_yojson |> Yojson.Safe.to_string
              ]] (),
          Helpers.build_path ~api:true ~route:(Router.SetLy slug) ()
        in
        let c_pdf, b_pdf, e_pdf, bass_pdf,
            ly
          =
          Inputs.Button.create ~href:(Lwt.return c_pdf_href) ~icon:"file-pdf" ~text:"PDF" page,
          Inputs.Button.create ~href:(Lwt.return b_pdf_href) ~icon:"file-pdf" ~text:"PDF (B♭)" page,
          Inputs.Button.create ~href:(Lwt.return e_pdf_href) ~icon:"file-pdf" ~text:"PDF (E♭)" page,
          Inputs.Button.create ~href:(Lwt.return bass_pdf_href) ~icon:"file-pdf" ~text:"PDF (𝄢)" page,
          Inputs.Button.create ~href:(Lwt.return ly_href) ~icon:"file-alt" ~text:"LilyPond" page
        in

        [
          node_of_dom_node (Inputs.Button.root c_pdf :> dom_node);
          node_of_dom_node (Inputs.Button.root b_pdf :> dom_node);
          node_of_dom_node (Inputs.Button.root e_pdf :> dom_node);
          node_of_dom_node (Inputs.Button.root bass_pdf :> dom_node);
          br;
          node_of_dom_node (Inputs.Button.root ly :> dom_node);
        ]
      );

      p [ text_lwt (
          match%lwt set_lwt >>=| Set.instructions with
          | "" -> Lwt.return ""
          | instructions -> Lwt.return ("Instructions: " ^ instructions)) ];

      div ~classes:["section"] [
        h3 [ text "Previsualisation" ];

        div_lwt (
          let%lwt set = set_lwt in
          let%lwt versions_and_parameters = Set.versions_and_parameters set in

          Lwt_list.map_p
            (fun (version, _parameters) ->
               (* FIXME: use parameters *)
               let%lwt tune = Version.tune version in
               let%lwt slug = Version.slug version in

               Lwt.return (div ~classes:["image-container"] [
                   (let href =
                      Router.path_of_controller (Router.Version slug) |> snd
                    in
                    h4 [ a ~href [ text_lwt (Tune.name tune) ] ]);

                   (let src =
                      spf "/%s%s"
                        Constant.api_prefix
                        (Router.path_of_controller (Router.VersionSvg slug) |> snd)
                    in
                    img ~src ())
                 ])
            )
            versions_and_parameters
        );
      ];

      div ~classes:["section"] [
        h3 [ text "Books in Which This Set Appears" ];

        div_lwt (
          let books_lwt =
            let%lwt set = set_lwt in
            let filter = Book.Filter.memSet set in
            Book.all ~filter ()
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              p [ text "There are no books containing this set." ]
            else
              Dancelor_client_tables.Book.make books
          ]
        )
      ]
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
