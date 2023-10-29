open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components
open Js_of_ocaml

let create slug =
  ModalBox.make [
    h2 ~a:[a_class ["title"]] [txt "Download a PDF"];


    let (key_choices, key_choices_signal) =
      Choices.(make [
          choice [txt "C"] ~checked:true;

          choice [txt "B♭"]
            ~value:(BookParameters.make_instrument (Music.make_pitch B Flat (-1)));

          choice [txt "E♭"]
            ~value:(BookParameters.make_instrument (Music.make_pitch E Flat 0));
        ])
    in

    let (clef_choices, clef_choices_signal) =
      Choices.(make [
          choice [txt "𝄞"] ~checked:true;

          choice [txt "𝄢"]
            ~value:(BookParameters.(make ~every_set:SetParameters.(
                make ~every_version:VersionParameters.(
                    make ~clef:Music.Bass ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1))) ()
                  ) ()) ()));
        ])
    in

    let (booklet_choices, booklet_choices_signal) =
      Choices.(make [
          choice [txt "Normal"] ~checked:true;

          choice [txt "Booklet"]
            ~value:(BookParameters.make
                      ~front_page:true
                      ~table_of_contents:End
                      ~two_sided:true
                      ~every_set:SetParameters.(make ~forced_pages:2 ())
                      ());
        ])
    in

    form [
      table [
        tr [td [label [txt "Key:"]]; td [key_choices]];
        tr [td [label [txt "Clef:"]]; td [clef_choices]];
        tr [td [label [txt "Mode:"]]; td [booklet_choices]];
      ];

      input
        ~a:[
          a_class ["button"];
          a_input_type `Submit;
          a_value "Download";
          a_onclick (fun _event ->
              let parameters = Option.concat_l BookParameters.compose [
                  S.value key_choices_signal;
                  S.value clef_choices_signal;
                  S.value booklet_choices_signal;
                ] in
              let href = ApiRouter.(path @@ bookPdf slug parameters) in
              ignore (Dom_html.window##open_ (Js.string href) (Js.string "_blank") Js.null);
              false
            );
        ] ();
    ];
  ]
