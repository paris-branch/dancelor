open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let dance_lwt = Dance.get slug in

  Lwt.async (fun () ->
      let%lwt dance = dance_lwt in
      let%lwt name = Dance.name dance in
      document##.title := js (name ^ " | Dance | Dancelor");
      Lwt.return ()
    );

  (
    let open Dancelor_client_html.NewAPI in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [
        R.txt @@ S.from' "" (dance_lwt >>=| Dance.name);
      ];
      R.h3 ~a:[a_class ["title"]] (
        RList.from_lwt' [] @@
        let kind = [
          R.txt @@ S.from' "" (dance_lwt >>=| Dance.kind >|=| Kind.Dance.to_pretty_string)
        ] in
        let%lwt by =
          match%lwt dance_lwt >>=| Dance.deviser with
          | None -> Lwt.return_nil
          | Some deviser ->
            let%lwt line = Formatters.CreditNewAPI.line ~link:true (Some deviser) in
            Lwt.return (txt " by " :: line)
        in
        Lwt.return (kind @ by)
      );
      R.div (
        RList.from_lwt' [] @@
        let%lwt dance = dance_lwt in
        match%lwt Dance.two_chords dance with
        | false -> Lwt.return_nil
        | true -> Lwt.return [h3 ~a:[a_class ["title"]] [txt "Two Chords"]]
      );
      R.div (
        RList.from_lwt' [] @@
        match%lwt dance_lwt >>=| Dance.scddb_id with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.dance_uri scddb_id in
          Lwt.return [
            h3 ~a:[a_class ["title"]] [
              a ~a:[a_href href; a_target "blank"] [
                txt "Link to the Strathspey Database"
              ]
            ]
          ]
      );

      div ~a:[a_class ["section"]] [
        h3 [txt "Recommended Tunes"];

        R.div (
          RList.from_lwt' [] @@
          let tunes_lwt =
            let%lwt dance = dance_lwt in
            let filter = Tune.Filter.existsDance (Dance.Filter.is dance) in
            Tune.search filter
            >|=| Score.list_erase
          in
          let%lwt tunes = tunes_lwt in

          Lwt.return [
            if tunes = [] then
              txt ("There are no recommended tunes for this dance. "
                   ^ "Dancelor is not all-knowing: go check the Strathspey Database! "
                   ^ "And if you find something that is not known here, report it to someone.")
            else
              Dancelor_client_tables.TheNewAPI.tunes tunes
          ]
        )
      ];
    ]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
