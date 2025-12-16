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
        ~label: "Edit"
        ~icon: "pencil-square"
        ~href: (S.const @@ Endpoints.Page.(href PersonEdit) id)
        ~dropdown: true
        ();
      Utils.Action.delete
        ~model: "person"
        ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Person Delete) (Entry.id person))
        ();
      ] @ (
        match Person.scddb_id' person with
        | None -> []
        | Some scddb_id -> [Utils.Action.scddb Person scddb_id]
      )
    )
    [
      div (
        if Model.Person.composed_tunes_are_public' person then
          [
            txt
              "This person indicates that the tunes they compose can be made \
               publicly available on Dancelor."
          ]
        else []
      );
      div (
        if Model.Person.published_tunes_are_public' person then
          [
            txt
              "This person indicates that the tunes they publish can be made \
               publicly available on Dancelor."
          ]
        else []
      );
      Utils.quick_explorer_links'
        (lwt person)
        [
          ("tunes they composed", Filter.(Any.tune' % Tune.existscomposer' % Person.is'));
          ("versions of tunes they composed", Filter.(Any.version' % Version.tune' % Tune.existscomposer' % Person.is'));
          ("dances they devised", Filter.(Any.dance' % Dance.existsdeviser' % Person.is'));
          ("sets they conceived", Filter.(Any.set' % Set.existsconceptor' % Person.is'));
          ("books they edited", Filter.(Any.book' % Book.existseditor' % Person.is'));
          ("sources they edited", Filter.(Any.source' % Source.existseditor' % Person.is'));
        ];
    ]
