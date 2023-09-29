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

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [

      h2 ~classes:["title"] const [
        text lwt (credit_lwt >>=| Credit.line)
      ];

      div ~classes:["section"] lwt (
        match%lwt credit_lwt >>=| Credit.scddb_id with
        | None -> Lwt.return_nil
        | Some scddb_id ->
          let href = const @@ SCDDB.person_uri scddb_id in
          Lwt.return [
            p const [
              text const "You can ";
              a ~href ~target:Blank const [
                text const "see this credit on the Strathspey Database"
              ];
              text const "."
            ]
          ]
      );

      div ~classes:["section"] const [
        h3 const [text const "Tunes Composed"];

        div lwt (
          let%lwt tunes =
            let%lwt credit = credit_lwt in
            let filter = Tune.Filter.authorIs credit in
            Tune.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if tunes = [] then
              text const "There are no tunes composed by this credit."
            else
              Dancelor_client_tables.tunes tunes
          ]
        );
      ];

      div ~classes:["section"] const [
        h3 const [text const "Sets Devised"];

        div lwt (
          let%lwt sets =
            let%lwt credit = credit_lwt in
            let filter = Set.Filter.deviser (Credit.Filter.is credit) in
            Set.search filter
            >|=| Score.list_erase
          in

          Lwt.return [
            if sets = [] then
              text const "There are no sets devised by this credit."
            else
              Dancelor_client_tables.sets sets
          ]
        );
      ];

      div ~classes:["section"] const [
        h3 const [text const "Dances Devised"];

        div lwt (
          let%lwt dances =
            let%lwt credit = credit_lwt in
            let filter = Dance.Filter.deviser (Credit.Filter.is credit) in
            Dance.search filter >|=| Score.list_erase
          in

          Lwt.return [
            if dances = [] then
              text const "There are no dances devised by this credit."
            else
              Dancelor_client_tables.dances dances
          ]
        );
      ]]);

  {page; content}

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
