open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_components
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
  let set_lwt = Set.get slug in

  Lwt.async (fun () ->
      let%lwt set = set_lwt in
      let%lwt title = Set.name set in
      document##.title := js (title ^ " | Set | Dancelor");
      Lwt.return ()
    );

  let open Dancelor_client_html in

  let (pdf_dialog, show_pdf_dialog) =
    ModalBox.make [
      h2 ~a:[a_class ["title"]] [txt "Download a PDF"];

      let (key_choices, key_choices_signal) =
        Choices.(make [
            choice [txt "C"] ~checked:true;

            choice [txt "B♭"]
              ~value:(SetParameters.make_instrument (Music.make_pitch B Flat (-1)));

            choice [txt "E♭"]
              ~value:(SetParameters.make_instrument (Music.make_pitch E Flat 0));
          ])
      in

      let (clef_choices, clef_choices_signal) =
        Choices.(make [
            choice [txt "𝄞"] ~checked:true;

            choice [txt "𝄢"]
              ~value:(SetParameters.(make ~every_version:VersionParameters.(
                  make ~clef:Music.Bass ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1))) ()
                ) ()));
          ])
      in

      form [
        table [
          tr [td [label [txt "Key:"]]; td [key_choices]];
          tr [td [label [txt "Clef:"]]; td [clef_choices]];
        ];

        input
          ~a:[
            a_class ["button"];
            a_input_type `Submit;
            a_value "Download";
            a_onclick (fun _event ->
                let parameters = Option.concat_l SetParameters.compose [
                    S.value key_choices_signal;
                    S.value clef_choices_signal;
                  ] in
                let href = ApiRouter.(path @@ setPdf slug parameters) in
                ignore (Dom_html.window##open_ (Js.string href) (Js.string "_blank") Js.null);
                false
              );
          ] ();
      ];
    ]
  in

  (
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [L.txt (set_lwt >>=| Set.name)];
      L.h3 ~a:[a_class ["title"]] (set_lwt >>=| Formatters.Set.works);
      h3 ~a:[a_class ["title"]] [
        L.txt (set_lwt >>=| Set.kind >|=| Kind.Dance.to_pretty_string);
        txt " — Play ";
        L.txt (set_lwt >>=| Set.order >|=| SetOrder.to_pretty_string);
      ];
      L.h3 ~a:[a_class ["title"]] (
        match%lwt set_lwt >>=| Set.deviser with
        | None -> Lwt.return_nil
        | Some deviser ->
          let%lwt line_block = Formatters.Credit.line ~link:true (Some deviser) in
          Lwt.return (txt "Set devised by " :: line_block)
      );

      pdf_dialog;

      div ~a:[a_class ["buttons"]] (

        let pdf_dialog_button =
          a
            ~a:[
              a_class ["button"];
              a_onclick (fun _ -> show_pdf_dialog (); false);
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
              a_href ApiRouter.(path @@ setLy slug @@ Option.none);
            ]
            [
              i ~a:[a_class ["fas"; "fa-file-alt"]] [];
              txt " LilyPond"
            ]
        in

        [
          pdf_dialog_button;
          ly_download_button;
        ]
      );

      p [ L.txt (
          match%lwt set_lwt >>=| Set.instructions with
          | "" -> Lwt.return ""
          | instructions -> Lwt.return ("Instructions: " ^ instructions)) ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Previsualisation"];

        L.div (
          let%lwt set = set_lwt in
          let%lwt versions_and_parameters = Set.versions_and_parameters set in

          Lwt_list.map_p
            (fun (version, _parameters) ->
               (* FIXME: use parameters *)
               let%lwt tune = Version.tune version in
               let%lwt slug = Version.slug version in

               Lwt.return (
                 div ~a:[a_class ["image-container"]]
                   [
                     h4 [a ~a:[a_href PageRouter.(path (Version slug))] [L.txt (Tune.name tune)]];

                     object_ ~a:[
                       a_mime_type "image/svg+xml";
                       a_data ApiRouter.(path (versionSvg slug None));
                     ] [];
                   ]
               )
            )
            versions_and_parameters
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Books in Which This Set Appears"];

        L.div (
          let books_lwt =
            let%lwt set = set_lwt in
            let filter = Book.Filter.memSet set in
            Book.search filter
            >|=| Score.list_erase
          in
          let%lwt books = books_lwt in

          Lwt.return [
            if books = [] then
              p [txt "There are no books containing this set."]
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
