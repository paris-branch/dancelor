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
            ~value:(VersionParameters.make ~transposition:(Transposition.relative (Music.make_pitch B Flat (-1)) Music.pitch_c) ());

          choice [txt "E♭"]
            ~value:(VersionParameters.make ~transposition:(Transposition.relative (Music.make_pitch E Flat 0) Music.pitch_c) ());
        ])
    in

    let (clef_choices, clef_choices_signal) =
      Choices.(make [
          choice [txt "𝄞"] ~checked:true;

          choice [txt "𝄢"]
            ~value:(VersionParameters.make ~clef:Music.Bass ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1))) ());
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
              let parameters = Option.concat_l VersionParameters.compose [
                  S.value key_choices_signal;
                  S.value clef_choices_signal;
                ] in
              let href = ApiRouter.(path @@ versionPdf slug parameters) in
              ignore (Dom_html.window##open_ (Js.string href) (Js.string "_blank") Js.null);
              false
            );
        ] ();
    ];
  ]
