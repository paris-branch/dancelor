open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_model

let js = Js.string

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  let credit_lwt = Credit.get slug in

  Lwt.async (fun () ->
      let%lwt credit = credit_lwt in
      let%lwt line = Credit.line credit in
      document##.title := js (line ^ " | Credit | Dancelor");
      Lwt.return ()
    );

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [
        L.txt (credit_lwt >>=| Credit.line);
      ];

      L.div ~a:[a_class ["section"]] (
        match%lwt credit_lwt >>=| Credit.scddb_id with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.person_uri scddb_id in
          Lwt.return [
            p [
              txt "You can ";
              a ~a:[a_href href; a_target "blank"] [
                txt "see this credit on the Strathspey Database"
              ];
              txt "."
            ]
          ]
      );

      div ~a:[a_class ["section"]] [
        h3 [txt "Tunes Composed"];

        L.div (
          let%lwt tunes =
            let%lwt credit = credit_lwt in
            let filter = Tune.Filter.authorIs credit in
            Tune.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if tunes = [] then
              txt "There are no tunes composed by this credit."
            else
              Dancelor_client_tables.tunes tunes
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Sets Devised"];

        L.div (
          let%lwt sets =
            let%lwt credit = credit_lwt in
            let filter = Set.Filter.deviser (Credit.Filter.is credit) in
            Set.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if sets = [] then
              txt "There are no sets devised by this credit."
            else
              Dancelor_client_tables.sets sets
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Dances Devised"];

        L.div (
          let%lwt dances =
            let%lwt credit = credit_lwt in
            let filter = Dance.Filter.deviser (Credit.Filter.is credit) in
            Dance.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if dances = [] then
              txt "There are no dances devised by this credit."
            else
              Dancelor_client_tables.dances dances
          ]
        );
      ]
    ]
  );

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
