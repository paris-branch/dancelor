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
        ~label: "Key"
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
        ~label: "Clef"
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
          S.map Result.get_ok (Component.signal key_choices);
          S.map Result.get_ok (Component.signal clef_choices);
        ]
  in
  {
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
      Utils.Button.download ~href: (S.map (fun params -> Endpoints.Api.(href @@ Version Pdf) (Entry.id version) slug params RenderingParameters.none) dialog.parameters_signal) ();
    ]

let create_and_open id = open_ id (create ())
