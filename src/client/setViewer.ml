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
  versions : Dom_html.uListElement Js.t;
}

let display_versions_and_parameters t versions_and_parameters =
  t.versions##.textContent := Js.null;
  List.iter
    (fun (version, _parameters) ->
       (* FIXME: use parameters *)
       let tune = Version.tune version in
       let slug = Version.slug version in
       let href =
         let%lwt slug = slug in
         Lwt.return (Router.path_of_controller (Router.Version slug) |> snd)
       in
       let name = Text.Link.create ~href ~text:(tune >>=| Tune.name) t.page in
       let title = Dom_html.createH4 (Page.document t.page) in
       Dom.appendChild title (Text.Link.root name);

       let source =
         Lwt.map (fun slug ->
             spf "/%s%s"
               Constant.api_prefix
               (Router.path_of_controller (Router.VersionSvg slug) |> snd))
           slug
       in
       let img = Image.create ~source t.page in
       Dom.appendChild t.versions title;
       Dom.appendChild t.versions (Image.root img))
    versions_and_parameters

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let set_lwt = Set.get slug in

  let versions = Dom_html.createUl (Page.document page) in
  versions##.textContent := Js.some (js "Loading tunes...");
  let t = {page; content; versions} in
  Lwt.on_success set_lwt (fun set -> Lwt.on_success (Set.versions_and_parameters set) (display_versions_and_parameters t));

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] [ text_lwt (set_lwt >>=| Set.name) ];
      h3 ~classes:["title"] [ text_lwt (Formatters.Set.works_lwt set_lwt) ];
      h3 ~classes:["title"] [ text_lwt (let open Lwt in set_lwt >>=| Set.kind >|= Kind.dance_to_pretty_string) ];
      h3_lwt ~classes:["title"] (
        let%lwt nodes =
          match%lwt set_lwt >>=| Set.deviser with
          | None -> Lwt.return []
          | Some deviser ->
            let%lwt line_block = Formatters.Credit.line (Some deviser) page in
            Lwt.return ((Formatters.text "Set devised by " page :> Dom.node Js.t) :: line_block)
        in
        Lwt.return (List.map node_of_dom_node nodes)
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

        div [
          (* FIXME *)
          node_of_dom_node (versions :> dom_node)
        ];
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
              text "There are no books containing this set."
            else
              node_of_dom_node
                (Table.root (Dancelor_client_tables.Book.make books_lwt page)
                 :> dom_node)
          ]
        )
      ]
    ]);

  t

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
