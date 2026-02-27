open Nes
open Common
open Model
open Html
open Utils

let create ?context id =
  Main_page.madge_call_or_404 (Source Get) id @@ fun source ->
  Page.make'
    ~parent_title: "Source"
    ~before_title: [
      Components.Context_links.make_and_render
        (* FIXME: doesn't need to take an [Lwt.t] anymore? *)
        ?context
        ~this_page: (Endpoints.Page.href_source id)
        (lwt @@ Any.source source);
    ]
    ~title: (lwt @@ NEString.to_string @@ Source.name' source)
    ~subtitles: [Formatters.Source.date_and_editors' source]
    ~share: (Source source)
    ~actions: [
      (
        match%lwt Permission.can_update_public source with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Button.make_a
              ~label: "Edit"
              ~icon: (Action Edit)
              ~href: (S.const @@ Endpoints.Page.(href Source_edit) id)
              ~dropdown: true
              ()
          ]
      );
      (
        match%lwt Permission.can_delete_public source with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Action.delete
              ~model: "source"
              ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Source Delete) (Entry.id source))
              ();
          ]
      );
      (lwt @@ Option.map_to_list (Action.scddb Publication) (Source.scddb_id' source));
    ]
    [
      div
        ~a: [a_class ["row"]]
        [
          div
            ~a: [a_class ["col-12"; "col-sm"]]
            [
              img ~a: [a_style "width: 100%;"] ~alt: "Cover" ~src: (Endpoints.Api.(href @@ Source Cover) id) ()
            ];
          div
            ~a: [a_class ["col-12"; "col-sm"]]
            [
              p
                [
                  txt
                    (
                      match Source.description' source with
                      | None -> "no description available"
                      | Some description -> description
                    )
                ];
            ];
        ];
      quick_explorer_links
        [
          ("versions from this source", lwt @@ Filter.(Any.version' % Version.sources' % Formula_list.exists' % Source.is') source);
        ];
    ]
