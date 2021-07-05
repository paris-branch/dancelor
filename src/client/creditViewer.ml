open Nes
open Js_of_ocaml
open Dancelor_client_elements
open Dancelor_client_model
open Dancelor_common

module Html = Dom_html

let js = Js.string

type t =
  {
    page : Page.t;
    content : Html.divElement Js.t;
  }

let create slug page =
  let document = Page.document page in
  let content = Html.createDiv document in
  let credit_lwt = Credit.get slug in

  Dancelor_client_html.(append_nodes (content :> dom_node) (Page.document page) [

      h2 ~classes:["title"] [
        text_lwt (credit_lwt >>=| Credit.line);
        text " (Credit)"
      ];

      div ~classes:["section"] [
        p_lwt (
          let%lwt credit = credit_lwt in
          let%lwt persons = Credit.persons credit in

          let link_of_person person =
            let slug = Person.slug person in
            let href_lwt =
              let%lwt slug = slug in
              Lwt.return (Router.path_of_controller (Router.Person slug) |> snd)
            in
            a ~href_lwt [ text_lwt (Person.name person) ]
          in

          Lwt.return (
            (text "This is the page of a credit containing ")

            :: match persons with

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

              [ link_of_person first_person ]
              @ List.concat_map (fun person -> [ text ", "; link_of_person person ]) persons
              @ [ text " and "; link_of_person last_person; text ".";
                  text " Visit the specific pages of the individual persons to";
                  text " see their personal work." ]
          )
        );
      ];

      div ~classes:["section"] [
        h3 [ text "Tunes Composed" ];

        div_lwt (
          let tunes_lwt =
            let%lwt credit = credit_lwt in
            let filter = Formula.(pred (Tune.Filter.Author (pred (Credit.Filter.Is credit)))) in
            Tune.all ~filter ()
          in
          let%lwt tunes = tunes_lwt in

          Lwt.return [
            if tunes = [] then
              text "There are no tunes composed by this credit."
            else
              node_of_dom_node (
                Table.root (Dancelor_client_tables.Tune.make tunes_lwt page)
                :> dom_node
              )
          ]
        );
      ];

      div ~classes:["section"] [
        h3 [ text "Sets Devised" ];

        div_lwt (
          let sets_lwt =
            let%lwt credit = credit_lwt in
            let filter = Set.Filter.deviser (Credit.Filter.is credit) in
            Set.all ~filter ()
          in
          let%lwt sets = sets_lwt in

          Lwt.return [
            if sets = [] then
              text "There are no sets containing this version."
            else
              node_of_dom_node (
                Table.root (Dancelor_client_tables.Set.make sets_lwt page)
                :> dom_node
              )
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
