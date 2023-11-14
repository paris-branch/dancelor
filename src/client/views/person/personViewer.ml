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
  let person_lwt = Person.get slug in

  Lwt.async (fun () ->
      let%lwt person = person_lwt in
      let%lwt line = Person.line person in
      document##.title := js (line ^ " | Person | Dancelor");
      Lwt.return ()
    );

  (
    let open Dancelor_client_html in
    Dom.appendChild content @@ To_dom.of_div @@ div [
      h2 ~a:[a_class ["title"]] [
        L.txt (person_lwt >>=| Person.line);
      ];

      L.div ~a:[a_class ["section"]] (
        match%lwt person_lwt >>=| Person.scddb_id with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = SCDDB.person_uri scddb_id in
          Lwt.return [
            p [
              txt "You can ";
              a ~a:[a_href href; a_target "blank"] [
                txt "see this person on the Strathspey Database"
              ];
              txt "."
            ]
          ]
      );

      div ~a:[a_class ["section"]] [
        h3 [txt "Tunes Composed"];

        L.div (
          let%lwt tunes =
            let%lwt person = person_lwt in
            let filter = Tune.Filter.authorIs person in
            Tune.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if tunes = [] then
              txt "There are no tunes composed by this person."
            else
              Dancelor_client_tables.tunes tunes
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Sets Devised"];

        L.div (
          let%lwt sets =
            let%lwt person = person_lwt in
            let filter = Set.Filter.deviser (Person.Filter.is person) in
            Set.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if sets = [] then
              txt "There are no sets devised by this person."
            else
              Dancelor_client_tables.sets sets
          ]
        );
      ];

      div ~a:[a_class ["section"]] [
        h3 [txt "Dances Devised"];

        L.div (
          let%lwt dances =
            let%lwt person = person_lwt in
            let filter = Dance.Filter.deviser (Person.Filter.is person) in
            Dance.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if dances = [] then
              txt "There are no dances devised by this person."
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
