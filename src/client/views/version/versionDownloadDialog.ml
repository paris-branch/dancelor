open Nes
open Dancelor_common
open Dancelor_client_model
open Dancelor_client_html
open Dancelor_client_components

type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: VersionParameters.t option React.signal;
}

let create () =
  let key_choices =
    Choices.(
      make_radios
        [
          choice' [txt "C"] ~checked: true;
          choice'
            [txt "B♭"]
            ~value: (VersionParameters.make_instrument (Music.make_pitch B Flat (-1)));
          choice'
            [txt "E♭"]
            ~value: (VersionParameters.make_instrument (Music.make_pitch E Flat 0));
        ]
    )
  in
  let clef_choices =
    Choices.(
      make_radios
        [
          choice' [txt "𝄞"] ~checked: true;
          choice'
            [txt "𝄢"]
            ~value: (VersionParameters.make ~clef: Music.Bass ~transposition: (Relative (Music.pitch_c, Music.make_pitch C Natural (-1))) ());
        ]
    )
  in

  (* A signal containing the composition of all the parameters. *)
  let parameters_signal =
    S.merge
      (Option.concat VersionParameters.compose)
      None
      [
        Choices.signal key_choices;
        Choices.signal clef_choices;
      ]
  in
  {
    choice_rows = [
      tr [td [label [txt "Key:"]]; td [Choices.render key_choices]];
      tr [td [label [txt "Clef:"]]; td [Choices.render clef_choices]];
    ];
    parameters_signal;
  }

let open_ slug dialog =
  Dialog.open_ @@ fun return ->
  [
    h2 ~a: [a_class ["title"]] [txt "Download a PDF"];
    form
      [
        table dialog.choice_rows;
        a
          ~a: [
            a_class ["button"];
            a_target "_blank";
            R.a_href (S.map (fun params -> ApiRouter.(path @@ route @@ Version Pdf) params slug) dialog.parameters_signal);
            a_onclick (fun _ -> return (); true);
          ]
          [txt "Download"];
      ];
  ]

let create_and_open slug = open_ slug (create ())
