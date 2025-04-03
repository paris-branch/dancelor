open Nes
open Common

open Model
open Html
open Components

type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: VersionParameters.t React.signal;
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
    S.map (Option.value ~default: VersionParameters.none) @@
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
  Page.open_dialog' @@ fun return ->
  Page.make
    ~title: (S.const "Download a PDF")
    [table dialog.choice_rows]
    ~buttons: [
      a
        ~a: [
          a_class ["button"];
          a_target "_blank";
          R.a_href (S.map (fun params -> Endpoints.Api.(href @@ Version Pdf) params slug) dialog.parameters_signal);
          a_onclick (fun _ -> return (); true);
        ]
        [txt "Download"];
    ]

let create_and_open slug = open_ slug (create ())
