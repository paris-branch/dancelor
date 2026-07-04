open Nes
open Dancelor_common
open Model
open Model_new
open Search_new
open Html
open Utils

let view context id =
  Main_page.madge_call_or_404 (Set Get_view) id @@ fun set ->
  Page.make'
    ~parent_title: "Set"
    ~before_title: [
      Components.Context_links.make_and_render_new
        ?context
        ~this_page: (Endpoints.Page.href_set id)
        (Any_id.Set id);
    ]
    ~title: (lwt set.name)
    ~subtitles: [
      span
        [
          txt (Kind.Dance.to_pretty_string set.kind);
          txt " — Play ";
          txt (Set_order.to_pretty_string set.order);
        ];
      span (
        match set.conceptors with
        | [] -> []
        | conceptors ->
          txt "Set by " :: Formatters_new.Person.names ~links: true conceptors
      );
    ]
    ~share_new: (Set id)
    ~actions: [
      lwt [
        Button.make
          ~label: "Download PDF"
          ~icon: (Other File_pdf)
          ~onclick: (fun _ -> ignore <$> Set_download_dialog.create_and_open set)
          ~dropdown: true
          ();
      ];
      (Add_to.button_to_book ~source_type: "set" ~source_format: Formatters_new.Set.name (Set_view.to_name set) (Model.Book.Set (id, Model.Set_parameters.none)));
      (
        (* FIXME: check permission to know whether to show this *)
        (* match%lwt Permission.can_update_private_new set with *)
        (* | None -> lwt_nil *)
        (* | Some _ -> *)
        lwt [
          Button.make_a
            ~label: "Edit"
            ~icon: (Action Edit)
            ~href: (S.const @@ Endpoints.Page.(href @@ Set Edit) id)
            ~dropdown: true
            ();
        ]
      );
      (
        (* FIXME: check permission to know whether to show this *)
        (* match%lwt Permission.can_delete_private_new set with *)
        (* | None -> lwt_nil *)
        (* | Some _ -> *)
        lwt [
          Action.delete
            ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Set Delete) id)
            ~model: "set"
            ();
        ]
      );
    ]
    [
      div
        (
          List.mapi
            (fun index (version, params) ->
              let context = Endpoints.Page.in_set id index in
              div
                ~a: [a_class ["mt-4"]]
                [
                  div ~a: [a_class ["row"; "justify-content-between"; "mb-2"]] [
                    div ~a: [a_class ["col"; "text-start"]] (
                      Formatters_new.Version.name_disambiguation_and_sources
                        ~context: (S.const context)
                        version @
                        Formatters_new.Version.parameters (Some params)
                    );
                    div ~a: [a_class ["col"; "text-end"]] (
                      Formatters_new.Version.composer_and_arranger
                        ~short: true
                        version @
                        Formatters_new.Version.display_composer (Some params)
                    );
                  ];
                  Components.Version_snippets.make ~show_audio: false ~params (Version_row.to_name version);
                ]
            )
            set.content
        );
      quick_explorer_links [
        ("books containing this set", Any_query.specific_only (Any_query.Book (Book_query.make_specific ~contains_set: (Some [id]) ())));
      ];
    ]
