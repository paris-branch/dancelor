open Nes
open Common
open Model
open Html
open Utils

let create ?context id =
  Main_page.madge_call_or_404 (Person Get) id @@ fun person ->
  Page.make'
    ~parent_title: "Person"
    ~before_title: [
      Components.Context_links.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_person id)
        (lwt @@ Any.person person);
    ]
    ~title: (lwt @@ NEString.to_string @@ Person.name' person)
    ~share: (Person person)
    ~actions: [
      lwt [
        Button.make_a
          ~label: "Edit"
          ~icon: (Action Edit)
          ~href: (S.const @@ Endpoints.Page.(href Person_edit) id)
          ~dropdown: true
          ();
        Action.delete
          ~model: "person"
          ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Person Delete) (Entry.id person))
          ();
      ];
      (lwt @@ Option.map_to_list (Action.scddb Person) (Person.scddb_id' person));
    ]
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
      quick_explorer_links'
        (lwt person)
        [
          ("tunes they composed", Filter.(Any.tune' % Tune.exists_composer' % Person.is'));
          ("versions of tunes they composed", Filter.(Any.version' % Version.tune' % Tune.exists_composer' % Person.is'));
          ("dances they devised", Filter.(Any.dance' % Dance.exists_deviser' % Person.is'));
          ("sets they conceived", Filter.(Any.set' % Set.exists_conceptor' % Person.is'));
          ("books they edited", Filter.(Any.book' % Book.exists_editor' % Person.is'));
          ("sources they edited", Filter.(Any.source' % Source.exists_editor' % Person.is'));
        ];
    ]
