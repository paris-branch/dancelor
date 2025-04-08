open Nes
open Common

open Model
open Html

let create ?context slug =
  let person_lwt = Person.get slug in
  let title = S.from' "" (Lwt.map Person.name person_lwt) in
  Page.make
    ~parent_title: "Person"
    ~title
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_person slug)
        (Lwt.map Any.person person_lwt);
    ]
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              L.li
                (
                  match%lwt Lwt.map Person.scddb_id person_lwt with
                  | None -> Lwt.return_nil
                  | Some scddb_id ->
                    Lwt.return
                      [
                        a
                          ~a: [
                            a_class ["dropdown-item"];
                            a_href (Uri.to_string @@ SCDDB.person_uri scddb_id);
                          ]
                          [
                            i ~a: [a_class ["bi"; "bi-box-arrow-up-right"]] [];
                            txt " See on SCDDB";
                          ]
                      ]
                );
            ];
        ];
      Utils.quick_explorer_links'
        person_lwt
        [
          ("tunes they composed", Any.Filter.tune' % Tune.Filter.existsComposer' % Person.Filter.is');
          ("dances they devised", Any.Filter.dance' % Dance.Filter.existsDeviser' % Person.Filter.is');
          ("sets they conceived", Any.Filter.set' % Set.Filter.existsConceptor' % Person.Filter.is');
        ];
    ]
