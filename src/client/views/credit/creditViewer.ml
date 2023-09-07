open Nes
open Js_of_ocaml
open Dancelor_common
open Dancelor_client_elements
open Dancelor_client_model

let js = Js.string

type t = {
  page: Page.t;
  content: Dom_html.divElement Js.t;
}

let create slug page =
  let document = Page.document page in
  let content = Dom_html.createDiv document in
  let credit_lwt = Credit.get slug in
  Lwt.async
    (
      fun () ->
        let%lwt credit = credit_lwt in
        let%lwt line = Credit.line credit in
        document##.title := js (line ^ " | Credit | Dancelor");
        Lwt.return ()
    );
  Dancelor_client_html.(append_nodes
                          (content :> dom_node)
                          (Page.document page)
                          [
                            h2
                              ~classes: ["title"]
                              [
                                text_lwt (credit_lwt >>=| Credit.line);
                                text " (Credit)"
                              ];
                            div_lwt
                              ~classes: ["section"]
                              (
                                match%lwt credit_lwt >>=| Credit.scddb_id with
                                | None -> Lwt.return_nil
                                | Some scddb_id ->
                                  let href = SCDDB.person_uri scddb_id in
                                  Lwt.return
                                    [
                                      p
                                        [
                                          text "You can ";
                                          a ~href ~target: Blank [text "see this credit on the Strathspey Database"];
                                          text "."
                                        ]
                                    ]
                              );
                            div
                              ~classes: ["section"]
                              [
                                p_lwt
                                  (
                                    let%lwt credit = credit_lwt in
                                    let%lwt persons = Credit.persons credit in
                                    let link_of_person person =
                                      let slug = Person.slug person in
                                      let href_lwt =
                                        let%lwt slug = slug in
                                        Lwt.return PageRouter.(path (Person slug))
                                      in
                                      a ~href_lwt [text_lwt (Person.name person)]
                                    in
                                    Lwt.return
                                      (
                                        (text "This is the page of a credit containing ") ::
                                        match persons with
                                        | [] ->
                                          [text "no persons."]
                                        | [person] ->
                                          let person = link_of_person person in
                                          [
                                            text "only the person ";
                                            person;
                                            text ". This page and the specific page of ";
                                            person;
                                            text " are different: the latter will contain all the work ";
                                            person;
                                            text " is involved in, while the former will contain only the work ";
                                            person;
                                            text " is involved in as this particular credit."
                                          ]
                                        | first_person :: persons ->
                                          let last_person = List.ft persons in
                                          let persons = List.bd persons in
                                          [link_of_person first_person] @
                                          List.concat_map (fun person -> [text ", "; link_of_person person]) persons @
                                          [
                                            text " and ";
                                            link_of_person last_person;
                                            text ".";
                                            text " Visit the specific pages of the individual persons to";
                                            text " see their personal work."
                                          ]
                                      )
                                  );
                              ];
                            div
                              ~classes: ["section"]
                              [
                                h3 [text "Tunes Composed"];
                                div_lwt
                                  (
                                    let%lwt tunes =
                                      let%lwt credit = credit_lwt in
                                      let filter = Tune.Filter.authorIs credit in
                                      Tune.search filter >|=| Score.list_erase
                                    in
                                    Lwt.return
                                      [
                                        if tunes = [] then
                                          text "There are no tunes composed by this credit."
                                        else
                                          Dancelor_client_tables.tunes tunes
                                      ]
                                  );
                              ];
                            div
                              ~classes: ["section"]
                              [
                                h3 [text "Sets Devised"];
                                div_lwt
                                  (
                                    let%lwt sets =
                                      let%lwt credit = credit_lwt in
                                      let filter = Set.Filter.deviser (Credit.Filter.is credit) in
                                      Set.search filter
                                      >|=| Score.list_erase
                                    in
                                    Lwt.return
                                      [
                                        if sets = [] then
                                          text "There are no sets devised by this credit."
                                        else
                                          Dancelor_client_tables.sets sets
                                      ]
                                  );
                              ];
                            div
                              ~classes: ["section"]
                              [
                                h3 [text "Dances Devised"];
                                div_lwt
                                  (
                                    let%lwt dances =
                                      let%lwt credit = credit_lwt in
                                      let filter = Dance.Filter.deviser (Credit.Filter.is credit) in
                                      Dance.search filter >|=| Score.list_erase
                                    in
                                    Lwt.return
                                      [
                                        if dances = [] then
                                          text "There are no dances devised by this credit."
                                        else
                                          Dancelor_client_tables.dances dances
                                      ]
                                  );
                              ]
                          ]);
  { page; content }

let contents t =
  t.content

let refresh t =
  ignore t

let init t =
  ignore t
