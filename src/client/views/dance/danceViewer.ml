open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t = {
  page: Page.t;
  content: Dom_html.divElement Js.t;
}

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let dance_lwt = Dance.get slug in
  Lwt.async
    (
      fun () ->
        let%lwt dance = dance_lwt in
        let%lwt name = Dance.name dance in
        document##.title := js (name ^ " | Dance | Dancelor");
        Lwt.return ()
    );
  Dancelor_client_html.(append_nodes
    (content :> dom_node)
    (Page.document page)
    [
      h2
        ~classes: ["title"]
        [
          text_lwt (dance_lwt >>=| Dance.name);
        ];
      h3_lwt
        ~classes: ["title"]
        (
          let kind =
            [
              text_lwt (dance_lwt >>=| Dance.kind >|=| Kind.dance_to_pretty_string);
            ]
          in
          let%lwt by =
            match%lwt dance_lwt >>=| Dance.deviser with
            | None -> Lwt.return_nil
            | Some deviser ->
              let%lwt line = Formatters.Credit.line ~link: true (Some deviser) in
              Lwt.return (text " by " :: line)
          in
          Lwt.return (kind @ by)
        );
      div_lwt
        (
          let%lwt dance = dance_lwt in
          match%lwt Dance.two_chords dance with
          | false -> Lwt.return_nil
          | true -> Lwt.return [h3 ~classes: ["title"] [text "Two Chords"]]
        );
      div_lwt
        (
          match%lwt dance_lwt >>=| Dance.scddb_id with
          | None -> Lwt.return_nil
          | Some scddb_id ->
            let href = SCDDB.dance_uri scddb_id in
            Lwt.return
              [
                h3
                  ~classes: ["title"]
                  [
                    a ~href ~target: Blank [text "Link to the Strathspey Database"];
                  ];
              ]
        );
      div
        ~classes: ["section"]
        [
          h3 [text "Recommended Tunes"];
          div_lwt
            (
              let tunes_lwt =
                let%lwt dance = dance_lwt in
                let filter = TuneFilter.existsDance (DanceFilter.is dance) in
                Tune.search filter
                >|=| Score.list_erase
              in
              let%lwt tunes = tunes_lwt in
              Lwt.return
                [
                  if tunes = [] then
                    text
                      (
                        "There are no recommended tunes for this dance. "
                        ^ "Dancelor is not all-knowing: go check the Strathspey Database! "
                        ^ "And if you find something that is not known here, report it to someone."
                      )
                  else
                    Dancelor_client_tables.tunes tunes;
                ]
            );
        ];
    ]);
  { page; content }

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
