open Nes
open Dancelor_common
open Dancelor_client_model
module Components = Dancelor_client_components
module Page = Dancelor_client_page
module Utils = Dancelor_client_utils
open Dancelor_client_html

let create ?context slug =
  let person_lwt = Person.get slug in
  let title = S.from' "" (Lwt.map Person.name person_lwt) in
  Page.make ~title: (Page.sub_title "Person" title) @@
  div
    [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (PageRouter.href_person slug)
        (Lwt.map Any.person person_lwt);
      h2 ~a: [a_class ["title"]] [R.txt title];
      L.div
        ~a: [a_class ["section"]]
        (
          match%lwt Lwt.map Person.scddb_id person_lwt with
          | None -> Lwt.return_nil
          | Some scddb_id ->
            let href = Uri.to_string @@ SCDDB.person_uri scddb_id in
            Lwt.return
              [
                p
                  [
                    txt "You can ";
                    a
                      ~a: [a_href href; a_target "blank"]
                      [
                        txt "see this person on the Strathspey Database"
                      ];
                    txt "."
                  ]
              ]
        );
      Utils.quick_explorer_links'
        person_lwt
        [
          ("tunes they composed", Any.Filter.tune' % Tune.Filter.existsComposer' % Person.Filter.is');
          ("dances they devised", Any.Filter.dance' % Dance.Filter.existsDeviser' % Person.Filter.is');
          ("sets they conceived", Any.Filter.set' % Set.Filter.existsConceptor' % Person.Filter.is');
        ];
    ]
