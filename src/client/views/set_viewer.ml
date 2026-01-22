open Nes
open Common
open Model
open Html
open Utils

let create ?context id =
  Main_page.madge_call_or_404 (Set Get) id @@ fun set ->
  Page.make'
    ~parent_title: "Set"
    ~before_title: [
      Components.Context_links.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_set id)
        (lwt @@ Any.set set);
    ]
    ~title: (lwt @@ NEString.to_string @@ Set.name' set)
    ~subtitles: [
      Formatters.Set.works' set;
      span
        [
          txt ((Kind.Dance.to_pretty_string % Set.kind') set);
          txt " — Play ";
          txt ((Set_order.to_pretty_string % Set.order') set);
        ];
      (
        with_span_placeholder @@
          match%lwt Set.conceptors' set with
          | [] -> lwt_nil
          | devisers -> lwt [txt "Set by "; Formatters.Person.names' ~links: true devisers]
      );
    ]
    ~share: (Set set)
    ~actions: [
      lwt [
        Button.make
          ~label: "Download PDF"
          ~icon: (Other File_pdf)
          ~onclick: (fun _ -> ignore <$> Set_download_dialog.create_and_open set)
          ~dropdown: true
          ();
        Button.make_a
          ~label: "Edit"
          ~icon: (Action Edit)
          ~href: (S.const @@ Endpoints.Page.(href Set_edit) id)
          ~dropdown: true
          ();
        Action.delete
          ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Set Delete) (Entry.id set))
          ~model: "set"
          ();
      ]
    ]
    [
      p
        [
          txt
            (
              match Set.instructions' set with
              | "" -> ""
              | instructions -> "Instructions: " ^ instructions
            )
        ];
      R.div
        (
          S.from'
            [
              div
                ~a: [a_class ["text-center"; "mt-4"]]
                [
                  h4 [span_placeholder ()];
                  div_placeholder ~min: 10 ~max: 20 ();
                ];
              div
                ~a: [a_class ["text-center"; "mt-4"]]
                [
                  h4 [span_placeholder ()];
                  div_placeholder ~min: 10 ~max: 20 ();
                ];
            ] @@
            let%lwt contents = Set.contents' set in
            Lwt_list.mapi_p
              (fun index (version, params) ->
                let context = Endpoints.Page.in_set id index in
                lwt @@
                  div
                    ~a: [a_class ["mt-4"]]
                    [
                      div ~a: [a_class ["row"; "justify-content-between"; "mb-2"]] [
                        div ~a: [a_class ["col-auto"; "text-start"]] [
                          Formatters.Version.name_disambiguation_and_sources'
                            ~context: (S.const context)
                            ~params
                            version
                        ];
                        div ~a: [a_class ["col-auto"; "text-end"]] [
                          Formatters.Version.composer_and_arranger'
                            ~short: true
                            ~params
                            version
                        ];
                      ];
                      Components.Version_snippets.make ~show_audio: false ~params version;
                    ]
              )
              contents
        );
      quick_explorer_links'
        (lwt set)
        [
          ("books containing this set", Filter.(Any.book' % Book.memset'));
        ];
    ]
