open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

let js = Js.string

type t =
  {
    page : Page.t;
    content : Dom_html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let credit_lwt = Credit.get slug in

  Lwt.async (fun () ->
      let%lwt credit = credit_lwt in
      let%lwt line = Credit.line credit in
      document##.title := js (line ^ " | Credit | Dancelor");
      Lwt.return ()
    );

  (
    let open Dancelor_client_html.NewAPI in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [
        R.txt (S.from' "" (credit_lwt >>=| Credit.line));
      ];

      R.div ~a:[a_class ["section"]] (
        RList.from_lwt' [] @@
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

        R.div (
          RList.from_lwt' [] @@
          let%lwt tunes =
            let%lwt credit = credit_lwt in
            let filter = Tune.Filter.authorIs credit in
            Tune.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if tunes = [] then
              txt "There are no tunes composed by this credit."
            else
              Dancelor_client_tables.TheNewAPI.tunes tunes
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Sets Devised"];

        R.div (
          RList.from_lwt' [] @@
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
              Dancelor_client_tables.TheNewAPI.sets sets
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Dances Devised"];

        R.div (
          RList.from_lwt' [] @@
          let%lwt dances =
            let%lwt credit = credit_lwt in
            let filter = Dance.Filter.deviser (Credit.Filter.is credit) in
            Dance.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if dances = [] then
              txt "There are no dances devised by this credit."
            else
              Dancelor_client_tables.TheNewAPI.dances dances
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
