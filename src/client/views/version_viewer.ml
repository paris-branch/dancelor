open Nes
open Common
open Model
open Html
open Utils

let show_lilypond_dialog version =
  let content_promise =
    let%lwt content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id version) in
    let content =
      match content with
      | Endpoints.Version.Protected -> assert false
      | Endpoints.Version.Granted {payload; _} -> payload
    in
    Model.Version.content_lilypond' ~content version
  in
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "LilyPond")
      [with_div_placeholder (
        let%lwt content = content_promise in
        lwt [pre [txt content]]
      )]
      ~buttons: [
        Button.close' ~return ();
        Button.make
          ~label: "Copy to clipboard"
          ~icon: (Other Clipboard)
          ~classes: ["btn-primary"]
          ~onclick: (fun _ ->
            let%lwt content = content_promise in
            write_to_clipboard content;
            Toast.open_ ~title: "Copied to clipboard" [txt "The LilyPond content was copied to your clipboard."];
            return (some ());
            lwt_unit
          )
          ()
      ]

let create ?context tune_id id =
  Main_page.madge_call_or_404 (Tune Get) tune_id @@ fun tune ->
  Main_page.madge_call_or_404 (Version Get) id @@ fun version ->
  let other_versions_promise =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Version Search)
        Slice.everything
        Formula.(
          and_l
            [
              Filter.Version.tuneis' tune;
              not (Filter.Version.is' version);
            ]
        )
  in
  Page.make'
    ~parent_title: "Version"
    ~before_title: [
      Components.Context_links.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_version tune_id id)
        (lwt @@ Any.version version);
    ]
    ~title: (NEString.to_string <$> Version.one_name' version)
    ~subtitles: [Formatters.Version.tune_description' version]
    ~share: (Version version)
    ~actions: (
      Lwt.l2
        (@)
        (
          lwt
            [
              Button.make
                ~label: "Download PDF"
                ~icon: (Other File_pdf)
                ~dropdown: true
                ~onclick: (fun _ -> ignore <$> Version_download_dialog.create_and_open version)
                ();
              Button.make
                ~label: "Show LilyPond"
                ~label_processing: "Showing LilyPond..."
                ~icon: (Other File_lilypond)
                ~dropdown: true
                ~onclick: (fun () -> show_lilypond_dialog version)
                ();
              Button.make_a
                ~label: "Edit"
                ~icon: (Action Edit)
                ~href: (S.const @@ Endpoints.Page.(href Version_edit) id)
                ~dropdown: true
                ();
              Button.make_a
                ~label: "Edit tune"
                ~icon: (Action Edit)
                ~href: (S.const @@ Endpoints.Page.(href Tune_edit) (Entry.id tune))
                ~dropdown: true
                ();
              Action.delete
                ~model: "version"
                ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Version Delete) (Entry.id version))
                ();
              Button.make
                ~label: "De-duplicate"
                ~icon: (Action Deduplicate)
                ~dropdown: true
                ~classes: ["btn-warning"]
                ~onclick: (Version_deduplicator.dialog ~version ~other_versions_promise)
                ()
            ]
        )
        (
          match%lwt Tune.scddb_id' <$> Model.Version.tune' version with
          | None -> lwt_nil
          | Some scddb_id -> lwt [Action.scddb Tune scddb_id]
        )
    )
    [
      div
        [
          with_span_placeholder @@
            match%lwt Tune.date' <$> Model.Version.tune' version with
            | None -> lwt_nil
            | Some date ->
              lwt [txtf "Composed %s." (PartialDate.to_pretty_string ~at: true date)]
        ];
      (
        (* For de-structured versions, show one of the common structures. *)
        div [
          div ~a: [a_class ["row"; "justify-content-between"]] [
            div ~a: [a_class ["col-auto"; "text-start"]] (
              let key = Model.Version.key' version in
              match Model.Version.content' version with
              | Monolithic {bars; structure; _} ->
                [
                  txtf
                    "%d-bar %s version in %s"
                    bars
                    (NEString.to_string @@ Model.Version.Structure.to_string structure)
                    (Music.Key.to_pretty_string key)
                ]
              | Destructured {default_structure; _} ->
                [
                  txt "Destructured version ";
                  Documentation.link "destructured-versions";
                  txtf
                    " in %s, shown here as %s"
                    (Music.Key.to_pretty_string key)
                    (NEString.to_string @@ Version.Structure.to_string default_structure)
                ]
            );
            div ~a: [a_class ["col-auto"; "text-end"]] [
              Formatters.Version.disambiguation' ~parentheses: false version;
              with_span_placeholder (
                match%lwt Model.Version.arrangers' version with
                | [] -> lwt_nil
                | arrangers ->
                  let name_block = Formatters.Person.names' ~links: true arrangers in
                  lwt ([txt " arranged by "; name_block])
              );
            ];
          ];
          (
            match Model.Version.content' version with
            | Monolithic _ -> Components.Version_snippets.make version
            | Destructured {default_structure; _} -> Components.Version_snippets.make version ~params: (Model.Version_parameters.make ~structure: default_structure ());
          )
        ];
      );
      R.div (
        S.from' [] @@
          match%lwt Model.Version.other_names' version with
          | [] -> lwt_nil
          | [other_name] -> lwt [txtf "Also known as %s." (NEString.to_string other_name)]
          | other_names ->
            lwt [
              txt "Also known as:";
              ul (List.map (li % List.singleton % txt % NEString.to_string) other_names);
            ]
      );
      R.div (
        S.from' [] @@
          let show_source_group source_group =
            let source = (List.hd source_group).Model.Version.source in
            span @@
            [Formatters.Source.name' source] @
            (
              List.concat @@
              List.interspersei
                (fun _ -> [txt ", "])
                ~last: (fun _ -> [txt " and "]) @@
              List.map
                (fun Model.Version.{details; structure; _} ->
                  [
                    (if details <> "" then txtf " %s" details else txt "");
                    txtf " as %s" (NEString.to_string (Model.Version.Structure.to_string structure));
                  ]
                )
                source_group
            ) @
              [txt "."]
          in
          match%lwt Model.Version.sources_grouped' version with
          | [] -> lwt_nil
          | [source_group] -> lwt [txt "Appears in "; show_source_group source_group]
          | source_groups ->
            lwt [
              txt "Appears:";
              ul (
                List.map
                  (fun source_group -> li [txt "in "; show_source_group source_group])
                  source_groups
              )
            ]
      );
      quick_explorer_links
        [
          ("sets containing this version", lwt @@ Filter.(Any.set' % Set.memversion') version);
          ("sets containing this tune", Filter.(Any.set' % Set.exists_version' % Version.tuneis') <$> Model.Version.tune' version);
          ("books containing this version", lwt @@ Filter.(Any.book' % Book.memversiondeep') version);
          ("books containing this tune", Filter.(Any.book' % Book.exists_version_deep' % Version.tuneis') <$> Model.Version.tune' version);
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Other Versions"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt other_versions = other_versions_promise in
                let%lwt tune_href = (Endpoints.Page.href_tune % Entry.id) <$> Model.Version.tune' version in
                lwt
                  (
                    if other_versions = [] then
                        [p [txt "There are no other versions available for this tune."]]
                    else
                      [
                        Tables.versions other_versions;
                        p
                          [
                            txt "You can also go to the ";
                            a ~a: [a_href tune_href] [txt "page of the tune"];
                            txt "."
                          ]
                      ]
                  )
            );
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances That Recommend This Tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt dances = Tune.dances' =<< Model.Version.tune' version in
                lwt
                  [
                    if dances = [] then
                      txt "There are no dances that recommend this tune."
                    else
                      Tables.dances dances
                  ]
            )
        ];
    ]
