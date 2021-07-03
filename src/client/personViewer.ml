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
  let person_lwt = Person.get slug in

  Dancelor_client_elements.H.(append_nodes (content :> dom_node) (Page.document page) [

      h2 ~classes:["title"] [
        text_lwt (person_lwt >>=| Person.name);
        text " (Person)"
      ];

      div ~classes:["section"] [
        p_lwt (
          let%lwt filter =
            let%lwt person = person_lwt in
            Lwt.return (Credit.Filter.memPerson person)
          in
          let%lwt credits = Credit.all ~filter () in

          let link_of_credit credit =
            let slug_lwt = Credit.slug credit in
            let href_lwt =
              let%lwt slug = slug_lwt in
              Lwt.return (Router.path_of_controller (Router.Credit slug) |> snd)
            in
            a ~href_lwt [ text_lwt (Credit.line credit) ]
          in

          Lwt.return (
            (text "This is the page of a person involved in ")

            :: match credits with
            | [] ->
              [text "no credits. Why this person exists in the database is a mystery!"]

            | [credit] ->
              [
                text "the sole credit ";
                link_of_credit credit;
                text "."
              ]

            | first_credit :: credits ->
              let last_credit = List.ft credits in
              let credits = List.bd credits in

              [ text "the credits "; link_of_credit first_credit ]
              @ List.concat_map (fun credit -> [ text ", ";  link_of_credit credit ]) credits
              @ [ text " and "; link_of_credit last_credit; text ".";
                  text " The current page will contain all the work this person";
                  text " is involved in. Visit the page of the credits for work";
                  text " done specifically as that particular credit." ]
          )
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Tunes Composed" ];

        div_lwt (
          let tunes_lwt =
            let%lwt person = person_lwt in
            let filter = Tune.Filter.author (Credit.Filter.memPerson person) in
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
        )
      ];

      div ~classes:["section"] [
        h3 [ text "Sets Devised" ];

        div_lwt (
          let sets_lwt =
            let%lwt person = person_lwt in
            let filter = Set.Filter.deviser (Credit.Filter.memPerson person) in
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
