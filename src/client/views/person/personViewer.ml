open Nes
open Dancelor_common
open Dancelor_client_model
module Components = Dancelor_client_components
module Page = Dancelor_client_page
open Dancelor_client_html

let create ?context slug =
  let person_lwt = Person.get slug in
  let title = S.from' "" (Lwt.map Person.name person_lwt) in
  Page.make_new_api ~title:(Page.sub_title "Person" title) @@
  div [
    Components.ContextLinks.make_and_render
      ?context
      ~this_page: (PageRouter.path_person slug)
      (Lwt.map Any.person person_lwt);

    h2 ~a:[a_class ["title"]] [R.txt title];

    L.div ~a:[a_class ["section"]] (
      match%lwt Lwt.map Person.scddb_id person_lwt with
      | None -> Lwt.return_nil
      | Some scddb_id ->
        let href = Uri.to_string @@ SCDDB.person_uri scddb_id in
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
          Tune.search' @@ Tune.Filter.existsComposerIs' person
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
          Set.search' @@ Set.Filter.existsConceptor' (Person.Filter.is' person)
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
          Dance.search' @@ Dance.Filter.existsDeviser' (Person.Filter.is' person)
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
