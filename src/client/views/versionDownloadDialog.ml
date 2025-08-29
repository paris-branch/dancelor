open Nes
open Common

open Model
open Html
open Components

type t = {
  choice_rows: Html_types.tr elt list;
  parameters_signal: (VersionParameters.t * RenderingParameters.t) React.signal;
}

(* TODO: Unicode flat in the rendering *)

let create () =
  let%lwt key_choices =
    Choices.(
      make_radios
        ~label: "Key"
        [
          choice' [txt "C"] ~checked: true;
          choice'
            [txt "B♭"]
            ~value: (
              VersionParameters.make ~transposition: (Transposition.relative (Music.make_pitch B Flat (-1)) Music.pitch_c) (),
              RenderingParameters.make ~instruments: "Bb instruments" ()
            );
          choice'
            [txt "E♭"]
            ~value: (
              VersionParameters.make ~transposition: (Transposition.relative (Music.make_pitch E Flat 0) Music.pitch_c) (),
              RenderingParameters.make ~instruments: "Eb instruments" ()
            );
        ]
    )
  in
  let%lwt clef_choices =
    Choices.(
      make_radios
        ~label: "Clef"
        [
          choice' [txt "𝄞"] ~checked: true;
          choice'
            [txt "𝄢"]
            ~value: (
              VersionParameters.make ~clef: Music.Bass ~transposition: (Relative (Music.pitch_c, Music.make_pitch C Natural (-1))) (),
              RenderingParameters.make ~clef: "bass clef" ()
            );
        ]
    )
  in
  (* A signal containing the composition of all the parameters. *)
  let parameters_signal =
    let no_parameters = (VersionParameters.none, RenderingParameters.none) in
    S.merge
      (Pair.map2_both VersionParameters.compose RenderingParameters.compose)
      no_parameters
      [
        S.map (Option.value ~default: no_parameters % Option.join % Result.to_option) (Component.signal key_choices);
        S.map (Option.value ~default: no_parameters % Option.join % Result.to_option) (Component.signal clef_choices);
      ]
  in
  lwt {
    choice_rows = [
      tr [td [label [txt "Key:"]]; td [Component.inner_html key_choices]];
      tr [td [label [txt "Clef:"]]; td [Component.inner_html clef_choices]];
    ];
    parameters_signal;
  }

let open_ version dialog =
  Page.open_dialog @@ fun return ->
  let%lwt slug = Version.slug' version in
  Page.make'
    ~title: (lwt "Download a PDF")
    [table dialog.choice_rows]
    ~buttons: [
      Utils.Button.cancel' ~return ();
      Utils.Button.download
        ~href: (S.map (uncurry @@ Endpoints.Api.(href @@ Version Pdf) (Entry.id version) slug) dialog.parameters_signal)
        ();
    ]

let create_and_open id = open_ id =<< create ()
