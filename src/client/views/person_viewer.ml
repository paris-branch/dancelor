open Nes
open Dancelor_common
open Model_new
open Html
open Utils

let view context id =
  Main_page.madge_call_or_404 (Person Get_view) id @@ fun person ->
  Page.make'
    ~parent_title: "Person"
    ~before_title: [
      Components.Context_links.make_and_render_new
        ?context
        ~this_page: (Endpoints.Page.href_person id)
        (Any_id.Person id);
    ]
    ~title: (lwt person.name)
    ~share_new: (Person id)
    ~actions: [
      (
        match%lwt Permission.can_update_public_new person with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Button.make_a
              ~label: "Edit"
              ~icon: (Action Edit)
              ~href: (S.const @@ Endpoints.Page.(href Person_edit) id)
              ~dropdown: true
              ();
          ]
      );
      (
        match%lwt Permission.can_delete_public_new person with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Action.delete
              ~model: "person"
              ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Person Delete) id)
              ();
          ]
      );
      (lwt @@ Option.map_to_list (Action.scddb Person) person.scddb_id);
    ]
    [
      div (
        if person.composed_tunes_are_public then
          [
            txt
              "This person indicates that the tunes they compose can be made \
               publicly available on Dancelor."
          ]
        else []
      );
      div (
        if person.published_tunes_are_public then
          [
            txt
              "This person indicates that the tunes they publish can be made \
               publicly available on Dancelor."
          ]
        else []
      );
      quick_explorer_links'
        (lwt person.id)
        [
          ("tunes they composed", Filter.(Any.tune' % Formula_entry.value' % Tune.composers' % Formula_list.exists' % Formula.pred % Formula_entry.is));
          ("versions of tunes they composed", Filter.(Any.version' % Formula_entry.value' % Version.tune' % Formula_entry.value' % Tune.composers' % Formula_list.exists' % Formula.pred % Formula_entry.is));
          ("dances they devised", Filter.(Any.dance' % Formula_entry.value' % Dance.devisers' % Formula_list.exists' % Formula.pred % Formula_entry.is));
          ("sets they conceived", Filter.(Any.set' % Formula_entry.value' % Set.conceptors' % Formula_list.exists' % Formula.pred % Formula_entry.is));
          ("books they edited", Filter.(Any.book' % Formula_entry.value' % Book.editors' % Formula_list.exists' % Formula.pred % Formula_entry.is));
          ("sources they edited", Filter.(Any.source' % Formula_entry.value' % Source.editors' % Formula_list.exists' % Formula.pred % Formula_entry.is));
        ];
    ]
