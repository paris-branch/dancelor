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

let madge_call_or_404_on_option route maybe_id =
  Option.fold
    maybe_id
    ~none: (fun f -> f None)
    ~some: (fun id f -> Main_page.madge_call_or_404 route id (f % some))

let create ?context tune_id id =
  Main_page.madge_call_or_404 (Tune Get) tune_id @@ fun tune ->
  madge_call_or_404_on_option (Version Get) id @@ fun specific_version ->
  let%lwt versions_of_this_tune =
    snd
    <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) Slice.everything @@
        Filter.Version.tuneis' tune
  in
  (* If no specific version was provided, grab any available one. FIXME: Some
     more logic here, eg. priorities books or versions that the user likes. *)
  let version =
    match specific_version with
    | Some version -> Some version
    | None -> List.hd_opt versions_of_this_tune
  in
  Page.make'
    ~parent_title: "Tune"
    ~before_title: [
      Components.Context_links.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_version tune_id id)
        (lwt @@ Option.fold specific_version ~some: Any.version ~none: (Any.tune tune));
    ]
    ~title: (lwt @@ NEString.to_string @@ Tune.one_name' tune)
    ~subtitles: [Formatters.Tune.description' tune]
    ~share: (Option.fold ~none: (Any.tune tune) ~some: Any.version version)
    ~actions: [
      (
        lwt @@
        Option.flip_map_to_list version @@ fun version ->
        Button.make
          ~label: "Download PDF"
          ~icon: (Other File_pdf)
          ~dropdown: true
          ~onclick: (fun _ -> ignore <$> Version_download_dialog.create_and_open version)
          ()
      );
      (
        lwt @@
        Option.flip_map_to_list version @@ fun version ->
        Button.make
          ~label: "Show LilyPond"
          ~label_processing: "Showing LilyPond..."
          ~icon: (Other File_lilypond)
          ~dropdown: true
          ~onclick: (fun () -> show_lilypond_dialog version)
          ()
      );
      (
        lwt @@
        Option.flip_map_to_list id @@ fun id ->
        Button.make_a
          ~label: "Edit version"
          ~icon: (Action Edit)
          ~href: (S.const @@ Endpoints.Page.(href Version_edit) id)
          ~dropdown: true
          ()
      );
      lwt [
        Button.make_a
          ~label: "Edit tune"
          ~icon: (Action Edit)
          ~href: (S.const @@ Endpoints.Page.(href Tune_edit) (Entry.id tune))
          ~dropdown: true
          ()
      ];
      (
        lwt @@
        Option.flip_map_to_list version @@ fun version ->
        Action.delete
          ~model: "version"
          ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Version Delete) (Entry.id version))
          ()
      );
      lwt [
        Action.delete
          ~model: "tune"
          ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Tune Delete) (Entry.id tune))
          ()
      ];
      (
        lwt @@
        Option.flip_map_to_list version @@ fun version ->
        Button.make
          ~label: "De-duplicate"
          ~icon: (Action Deduplicate)
          ~dropdown: true
          ~classes: ["btn-warning"]
          ~onclick: (Version_deduplicator.dialog ~tune ~version)
          ()
      );
      (lwt @@ Option.to_list @@ Option.map (Action.scddb Tune) (Tune.scddb_id' tune));
    ]
    [
      div
        (
          match Tune.date' tune with
          | None -> []
          | Some date -> [txtf "Composed %s." (PartialDate.to_pretty_string ~at: true date)]
        );
      (
        div @@
        Option.value ~default: [] @@
        Option.flip_map version @@ fun version ->
        [div ~a: [a_class ["row"; "justify-content-between"]] [
          div ~a: [a_class ["col-auto"; "text-start"]] (
            (
              let key = Model.Version.key' version in
              match Model.Version.content' version with
              | No_content -> []
              | Monolithic {bars; structure; _} ->
                [
                  txtf
                    "Monolithic %d-bar %s version in %s"
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
            ) @
            (
              match id with
              | Some _ -> []
              | None -> [txt ", selected by Dancelor"]
            ) @
              [txt "."]
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
          | No_content -> div [Alert.make ~level: Info [txt "This version does not have any content."]]
          | Monolithic _ -> Components.Version_snippets.make version
          | Destructured {default_structure; _} -> Components.Version_snippets.make version ~params: (Model.Version_parameters.make ~structure: default_structure ());
        )];
      );
      div (
        match Model.Tune.other_names' tune with
        | [] -> []
        | [other_name] -> [txtf "Also known as %s." (NEString.to_string other_name)]
        | other_names ->
          [
            txt "Also known as:";
            ul (List.map (li % List.singleton % txt % NEString.to_string) other_names);
          ]
      );
      R.div (
        Option.fold
          version
          ~none: (S.const [])
          ~some: (fun version ->
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
              | [source_group] -> lwt [txt "This specific version appears in "; show_source_group source_group]
              | source_groups ->
                lwt [
                  txt "This specific version appears:";
                  ul (
                    List.map
                      (fun source_group -> li [txt "in "; show_source_group source_group])
                      source_groups
                  )
                ]
          )
      );
      quick_explorer_links @@
        List.filter_map Fun.id [
          Option.flip_map version (fun version -> ("sets containing this version", lwt @@ Filter.(Any.set' % Set.memversion') version));
          Some ("sets containing this tune", Filter.(Any.set' % Set.exists_version' % Version.tuneis') <$> lwt tune);
          Option.flip_map version (fun version -> ("books containing this version", lwt @@ Filter.(Any.book' % Book.memversiondeep') version));
          Some ("books containing this tune", Filter.(Any.book' % Book.exists_version_deep' % Version.tuneis') <$> lwt tune);
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Versions of this tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt versions =
                  snd
                  <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) Slice.everything @@
                      Filter.Version.tuneis' tune
                in
                lwt @@
                  if versions = [] then
                      [txt "There are no versions for this tune."]
                  else
                      [Tables.versions versions]
            )
        ];
      div
        ~a: [a_class ["section"]]
        [
          h3 [txt "Dances that recommend this tune"];
          R.div
            (
              S.from' (Tables.placeholder ()) @@
                let%lwt dances = Tune.dances' tune in
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
