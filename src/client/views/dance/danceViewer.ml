open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model
module Formatters = Dancelor_client_formatters

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let dance_lwt = Dance.get slug in

  Lwt.async (fun () ->
      Fun.flip Lwt.map dance_lwt @@ fun dance ->
      document##.title := js (Dance.name dance ^ " | Dance | Dancelor")
    );

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [
        L.txt (Lwt.map Dance.name dance_lwt);
      ];
      L.h3 ~a:[a_class ["title"]] (
        let kind = [L.txt @@ Lwt.map (Kind.Dance.to_pretty_string % Dance.kind) dance_lwt] in
        let%lwt by =
          match%lwt dance_lwt >>=| Dance.deviser with
          | None -> Lwt.return_nil
          | Some deviser -> Lwt.return (txt " by " :: Formatters.Person.name ~link:true (Some deviser))
        in
        Lwt.return (kind @ by)
      );
      L.div (
        Fun.flip Lwt.map dance_lwt @@ fun dance ->
        match Dance.two_chords dance with
        | false -> []
        | true -> [h3 ~a:[a_class ["title"]] [txt "Two Chords"]]
      );
      L.div (
        match%lwt Lwt.map Dance.scddb_id dance_lwt with
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

        L.div (
          let tunes_lwt =
            let%lwt dance = dance_lwt in
            let filter = Tune.Filter.existsDance (Dance.Filter.is dance) in
            Tune.search' filter >|=| Score.list_erase
          in
          let%lwt tunes = tunes_lwt in

          Lwt.return [
            if tunes = [] then
              txt ("There are no recommended tunes for this dance. "
                   ^ "Dancelor is not all-knowing: go check the Strathspey Database! "
                   ^ "And if you find something that is not known here, report it to someone.")
            else
              Dancelor_client_tables.tunes tunes
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
