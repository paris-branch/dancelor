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
    ~title: (lwt @@ NEString.to_string @@ Person.name' person)
    ~share: (Person person)
    ~actions: (
      lwt @@
      [Utils.Button.make_a
        ~classes: ["dropdown-item"]
        ~href: (S.const @@ Endpoints.Page.(href PersonEdit) id)
        ~icon: "pencil-square"
        ~label: "Edit"
        ()] @ (
        match Person.scddb_id' person with
        | None -> []
        | Some scddb_id ->
          [
            Utils.Button.make_a
              ~label: "See on SCDDB"
              ~icon: "box-arrow-up-right"
              ~classes: ["dropdown-item"]
              ~href: (S.const @@ Uri.to_string @@ SCDDB.person_uri scddb_id)
              ()
          ]
      )
    )
    [
      Utils.quick_explorer_links'
        (lwt person)
        [
          ("tunes they composed", Filter.(Any.tune' % Tune.existscomposer' % Person.is'));
          ("dances they devised", Filter.(Any.dance' % Dance.existsdeviser' % Person.is'));
          ("sets they conceived", Filter.(Any.set' % Set.existsconceptor' % Person.is'));
          ("books they edited", Filter.(Any.book' % Book.existseditor' % Person.is'));
          ("sources they edited", Filter.(Any.source' % Source.existseditor' % Person.is'));
        ];
    ]
