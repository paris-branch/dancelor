open Js_of_ocaml
open Nes
open Common
open Model
open Html
open Utils
open Components

type t = {
  choice_rows: Html_types.div elt list;
  parameters_signal: (Version_parameters.t * Rendering_parameters.t) React.signal;
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
              Version_parameters.make ~transposition: (Transposition.from_semitones 2) (),
              Rendering_parameters.make ~instruments: "Bb instruments" ()
            );
          choice'
            [txt "E♭"]
            ~value: (
              Version_parameters.make ~transposition: (Transposition.from_semitones (-3)) (),
              Rendering_parameters.make ~instruments: "Eb instruments" ()
            );
        ]
    )
  in
  let%lwt clef_choices =
    Choices.(
      make_radios
        ~label: "Clef"
        [
          choice' [txt @@ Music.Clef.(to_symbol Treble)] ~checked: true;
          choice'
            [txt @@ Music.Clef.(to_symbol Bass)]
            ~value: (
              Version_parameters.make ~clef: Bass ~transposition: (Transposition.from_semitones (-12)) (),
              Rendering_parameters.make ~clef: "bass clef" ()
            );
        ]
    )
  in
  (* A signal containing the composition of all the parameters. *)
  let parameters_signal =
    let no_parameters = (Version_parameters.none, Rendering_parameters.none) in
    S.merge
      (Pair.map2 Version_parameters.compose Rendering_parameters.compose)
      no_parameters
      [
        S.map (Option.value ~default: no_parameters % Option.join % Result.to_option) (Component.signal key_choices);
        S.map (Option.value ~default: no_parameters % Option.join % Result.to_option) (Component.signal clef_choices);
      ]
  in
  lwt {
    choice_rows = [
      Component.html key_choices;
      Component.html clef_choices;
    ];
    parameters_signal;
  }

let open_pdf_generation_dialog status_signal =
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Download a PDF")
      [R.div (
        Job.show_live_status
          status_signal
          ~on_succeeded: (fun href ->
            Dom_html.window##.location##.href := Js.string (Uri.to_string href);
            [txt
              "The document generation job succeeded. You will be redirected \
                soon to: ";
            a ~a: [a_href href] [txt @@ Uri.to_string href];
            ]
          )
      )]
      ~buttons: [Button.cancel' ~return ()]

let copyright_reponse_promise_to_job_registration_promise copyright_response_promise =
  match%lwt copyright_response_promise with
  | Error error -> raise (Madge_client.Error error)
  | Ok Endpoints.Version.Protected -> lwt_none
  | Ok Endpoints.Version.Granted {payload; _} -> lwt_some payload

let open_ version dialog =
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Download a PDF")
    [div dialog.choice_rows]
    ~buttons: [
      Button.cancel' ~return ();
      Button.download
        ~onclick: (fun () ->
          let (version_params, rendering_params) = S.value dialog.parameters_signal in
          let%lwt slug = Version.slug' version in
          return None;
          open_pdf_generation_dialog (
            let copyright_response_promise =
              Madge_client.call
                Endpoints.Api.(route @@ Version Build_pdf)
                (Entry.id version)
                version_params
                rendering_params
            in
            let job_registration_promise = copyright_reponse_promise_to_job_registration_promise copyright_response_promise in
            let slug = NesSlug.add_suffix slug ".pdf" in
            Job.run3 slug job_registration_promise
          )
        )
        ();
    ]

let create_and_open id = open_ id =<< create ()
