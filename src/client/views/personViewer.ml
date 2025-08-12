open Nes
open Common

open Model
open Html

let create ?context id =
  MainPage.madge_call_or_404 (Person Get) id @@ fun person ->
  Page.make'
    ~parent_title: "Person"
    ~before_title: [
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_person id)
        (lwt @@ Any.person person);
    ]
    ~title: (lwt @@ Person.name' person)
    [
      div
        ~a: [a_class ["text-end"; "dropdown"]]
        [
          button ~a: [a_class ["btn"; "btn-secondary"; "dropdown-toggle"]; a_button_type `Button; a_user_data "bs-toggle" "dropdown"; a_aria "expanded" ["false"]] [txt "Actions"];
          ul
            ~a: [a_class ["dropdown-menu"]]
            [
              li
                (
                  match Person.scddb_id' person with
                  | None -> []
                  | Some scddb_id ->
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
        (lwt person)
        [
          ("tunes they composed", Filter.(Any.tune' % Tune.existsComposer' % Person.is'));
          ("dances they devised", Filter.(Any.dance' % Dance.existsDeviser' % Person.is'));
          ("sets they conceived", Filter.(Any.set' % Set.existsConceptor' % Person.is'));
        ];
    ]
