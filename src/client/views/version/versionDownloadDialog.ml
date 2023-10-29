open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components
open Js_of_ocaml

type t =
  {
    choice_rows : Html_types.tr elt list;
    parameters_signal : VersionParameters.t option React.signal;
  }

let create () =
  let (key_choices, key_choices_signal) =
    Choices.(make [
        choice [txt "C"] ~checked:true;

        choice [txt "B♭"]
          ~value:(VersionParameters.make_instrument (Music.make_pitch B Flat (-1)));

        choice [txt "E♭"]
          ~value:(VersionParameters.make_instrument (Music.make_pitch E Flat 0));
      ])
  in

  let (clef_choices, clef_choices_signal) =
    Choices.(make [
        choice [txt "𝄞"] ~checked:true;

        choice [txt "𝄢"]
          ~value:(VersionParameters.make ~clef:Music.Bass ~transposition:(Relative(Music.pitch_c, Music.make_pitch C Natural (-1))) ());
      ])
  in

  (* A signal containing the composition of all the parameters. *)
  let parameters_signal =
    S.merge (Option.concat VersionParameters.compose) None [
      key_choices_signal;
      clef_choices_signal;
    ]
  in

  {
    choice_rows = [
      tr [td [label [txt "Key:"]]; td [key_choices]];
      tr [td [label [txt "Clef:"]]; td [clef_choices]];
    ];
    parameters_signal;
  }

let render slug dialog =
  ModalBox.make [
    h2 ~a:[a_class ["title"]] [txt "Download a PDF"];

    form [
      table dialog.choice_rows;

      input
        ~a:[
          a_class ["button"];
          a_input_type `Submit;
          a_value "Download";
          a_onclick (fun _event ->
              let parameters = S.value dialog.parameters_signal in
              let href = ApiRouter.(path @@ versionPdf slug parameters) in
              ignore (Dom_html.window##open_ (Js.string href) (Js.string "_blank") Js.null);
              false
            );
        ] ();
    ];
  ]

let create_and_render slug = render slug (create ())
