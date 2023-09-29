open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
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

  Lwt.async (fun () ->
      let%lwt set = set_lwt in
      let%lwt title = Set.name set in
      document##.title := js (title ^ " | Set | Dancelor");
      Lwt.return ()
    );

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [
      h2 ~classes:["title"] const [text lwt (set_lwt >>=| Set.name)];
      h3 ~classes:["title"] lwt (set_lwt >>=| Formatters.Set.works);
      h3 ~classes:["title"] const [
        text lwt (set_lwt >>=| Set.kind >|=| Kind.Dance.to_pretty_string);
        text const " — Play ";
        text lwt (set_lwt >>=| Set.order >|=| SetOrder.to_pretty_string)
      ];
      h3 ~classes:["title"] lwt (
        match%lwt set_lwt >>=| Set.deviser with
        | None -> Lwt.return_nil
        | Some deviser ->
          let%lwt line_block = Formatters.Credit.line ~link:true (Some deviser) in
          Lwt.return (text const "Set devised by " :: line_block)
      );

      div ~classes:["buttons"] const (
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
          ApiRouter.(path @@ setPdf slug @@ Option.none),
          ApiRouter.(path @@ setPdf slug @@ Option.some    b_parameters),
          ApiRouter.(path @@ setPdf slug @@ Option.some    e_parameters),
          ApiRouter.(path @@ setPdf slug @@ Option.some bass_parameters),
          ApiRouter.(path @@ setLy  slug @@ Option.none)
        in

        let pdf_button href txt =
          a ~classes:["button"] ~href ~target:Blank const [
            i ~classes:["fas"; "fa-file-pdf"] const [];
            text const (" " ^ txt)
          ]
        in
        [
          pdf_button c_pdf_href    "PDF";
          pdf_button b_pdf_href    "PDF (B♭)";
          pdf_button e_pdf_href    "PDF (E♭)";
          pdf_button bass_pdf_href "PDF (𝄢)";
          br;
          a ~classes:["button"] ~href:ly_href const [
            i ~classes:["fas"; "fa-file-alt"] const [];
            text const " LilyPond"
          ];
        ]
      );

      p const [text lwt (
          match%lwt set_lwt >>=| Set.instructions with
          | "" -> Lwt.return ""
          | instructions -> Lwt.return ("Instructions: " ^ instructions)
        )];

      div ~classes:["section"] const [
        h3 const [text const "Previsualisation"];

        div lwt (
          let%lwt set = set_lwt in
          let%lwt versions_and_parameters = Set.versions_and_parameters set in

          Lwt_list.map_p
            (fun (version, _parameters) ->
               (* FIXME: use parameters *)
               let%lwt tune = Version.tune version in
               let%lwt slug = Version.slug version in

               Lwt.return (div ~classes:["image-container"] const [
                   (let href = PageRouter.(path (Version slug)) in
                    h4 const [a ~href const [text lwt (Tune.name tune)]]);

                   (let data = ApiRouter.(path (versionSvg slug None)) in
                    object_ ~type_:"image/svg+xml" ~data const [])
                 ])
            )
            versions_and_parameters
        );
      ];

      div ~classes:["section"] const [
        h3 const [text const "Books in Which This Set Appears"];

        div lwt (
          let books_lwt =
            let%lwt set = set_lwt in
            let filter = Book.Filter.memSet set in
            Book.search filter
            >|=| Score.list_erase
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              p const [text const "There are no books containing this set."]
            else
              Dancelor_client_tables.books books
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
