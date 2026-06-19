open Nes
open Dancelor_common
open Model_new
open Html
open Utils

let view context id =
  Main_page.madge_call_or_404 (Dance Get_view) id @@ fun dance ->
  Page.make'
    ~parent_title: "Dance"
    ~before_title: [
      Components.Context_links.make_and_render_new
        ?context
        ~this_page: (Endpoints.Page.href_dance id)
        (Any_id.Dance id);
    ]
    ~title: (lwt dance.name)
    ~subtitles: [
      span (Formatters_new.Dance.aka dance);
      span (Formatters_new.Dance.description dance);
    ]
    ~share_new: (Dance id)
    ~actions: [
      (Add_to.button_to_book ~source_type: "dance" ~source_format: Formatters_new.Dance.name (Dance_view.to_name dance) (Model.Book.Dance (id, Dance_only)));
      (
        match%lwt Permission.can_update_public_new dance with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Button.make_a
              ~label: "Edit"
              ~icon: (Action Edit)
              ~href: (S.const @@ Endpoints.Page.(href Dance_edit) id)
              ~dropdown: true
              ();
          ]
      );
      (
        match%lwt Permission.can_delete_public_new dance with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Action.delete
              ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Dance Delete) id)
              ~model: "dance"
              ();
          ]
      );
      (lwt @@ Option.map_to_list (Action.scddb Dance) dance.scddb_id);
    ]
    [
      div
        (
          match dance.two_chords with
          | One_chord -> []
          | Two_chords -> [txt "Two Chords"]
          | Dont_know -> [txt "Two Chords: unknown"]
        );
      div
        (
          match dance.date with
          | None -> []
          | Some date -> [txt "Devised "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        );
      div
        [
          h3 [txt "Recommended Tunes"];
          R.div
            (
              S.from_lwt (Tables.placeholder ()) @@
                let%lwt tunes = Madge_client.call_exn Endpoints.Api.(route @@ Dance Tunes) id in
                (* FIXME: include tunes in Dance_view.t *)
                lwt
                  [
                    if tunes = [] then
                      txt
                        (
                          "There are no recommended tunes for this dance. " ^
                          "Dancelor is not all-knowing: go check the Strathspey Database! " ^
                          "And if you find something that is not known here, report it to someone."
                        )
                    else
                      Tables.tunes tunes
                  ]
            )
        ];
    ]
