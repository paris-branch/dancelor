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

let add_to_set_dialog version user =
  Add_to.dialog
    user
    version
    ~source_type: "version"
    ~source_format: Formatters.Version.name'
    ~target_type: "set"
    ~target_icon: Icon.(Model Set)
    ~target_descr: Model.Set.name'
    ~target_format: Formatters.Set.name'
    ~target_href: Endpoints.Page.href_set
    ~target_create_editor: Set_editor.create
    ~target_filter_from_string: Filter.Set.from_string
    ~target_filter_owners': Filter.Set.owners'
    ~target_result: (Any_result.make_set_result ?context: None ?params: None)
    ~target_search: (Madge_client.call_exn Endpoints.Api.(route @@ Set Search))
    ~target_update: (Madge_client.call_exn Endpoints.Api.(route @@ Set Update))
    ~target_get: Model.Set.get
    ~target_add_source_to_content: (fun set ->
      let%lwt contents = Model.Set.contents set in
      lwt @@ Model.Set.set_contents (contents @ [(version, Model.Version_parameters.none)]) set
    )

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
        Option.to_list @@
        Option.bind version @@ fun version ->
        match Version.content' version with
        | No_content -> None
        | _ ->
          some @@
            Button.make
              ~label: "Download PDF"
              ~icon: (Other File_pdf)
              ~dropdown: true
              ~onclick: (fun _ -> ignore <$> Version_download_dialog.create_and_open version)
              ()
      );
      (
        lwt @@
        Option.to_list @@
        Option.bind version @@ fun version ->
        match Version.content' version with
        | No_content -> None
        | _ ->
          some @@
            Button.make
              ~label: "Show LilyPond"
              ~label_processing: "Showing LilyPond..."
              ~icon: (Other File_lilypond)
              ~dropdown: true
              ~onclick: (fun () -> show_lilypond_dialog version)
              ()
      );
      (
        match version with
        | None -> lwt_nil
        | Some version ->
          Lwt.l2
            (@)
            (Add_to.button ~target_type: "set" (add_to_set_dialog version))
            (
              Add_to.button_to_book
                ~source_type: "version"
                ~source_format: Formatters.Version.name'
                version
                (Model.Book.versions @@ NEList.singleton (version, Model.Version_parameters.none))
            )
      );
      (
        Option.fold
          version
          ~none: lwt_nil
          ~some: (fun version ->
            match%lwt Permission.can_update_public version with
            | None -> lwt_nil
            | Some _ ->
              lwt [
                Button.make_a
                  ~label: "Edit version"
                  ~icon: (Action Edit)
                  ~href: (S.const @@ Endpoints.Page.(href Version_edit) (Entry.id version))
                  ~dropdown: true
                  ()
              ]
          )
      );
      (
        match%lwt Permission.can_update_public tune with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Button.make_a
              ~label: "Edit tune"
              ~icon: (Action Edit)
              ~href: (S.const @@ Endpoints.Page.(href Tune_edit) (Entry.id tune))
              ~dropdown: true
              ()
          ]
      );
      (
        Option.fold
          version
          ~none: lwt_nil
          ~some: (fun version ->
            match%lwt Permission.can_delete_public version with
            | None -> lwt_nil
            | Some _ ->
              lwt [
                Action.delete
                  ~label_suffix: "version"
                  ~model: "version"
                  ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Version Delete) (Entry.id version))
                  ()
              ]
          )
      );
      (
        match%lwt Permission.can_delete_public tune with
        | None -> lwt_nil
        | Some _ ->
          lwt [
            Action.delete
              ~label_suffix: "tune"
              ~model: "tune"
              ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Tune Delete) (Entry.id tune))
              ()
          ]
      );
      (
        Option.fold
          version
          ~none: lwt_nil
          ~some: (fun version ->
            match%lwt Permission.can_administrate () with
            | false -> lwt_nil
            | true ->
              lwt [
                Button.make
                  ~label: "De-duplicate"
                  ~icon: (Action Deduplicate)
                  ~dropdown: true
                  ~classes: ["btn-warning"]
                  ~onclick: (Version_deduplicator.dialog ~tune ~version)
                  ()
              ]
          )
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
            let selected_by_dancelor =
              match id with
              | Some _ -> txt "."
              | None -> txt ", selected by Dancelor."
            in
            let key = Model.Version.key' version in
            match Model.Version.content' version with
            | No_content -> []
            | Monolithic {bars; structure; _} ->
              [
                txtf
                  "Monolithic %d-bar %s version in %s"
                  bars
                  (NEString.to_string @@ Model.Version.Structure.to_string structure)
                  (Music.Key.to_pretty_string key);
                selected_by_dancelor;
              ]
            | Destructured {default_structure; _} ->
              [
                txt "Destructured version ";
                Documentation.link "destructured-versions";
                txtf
                  " in %s, shown here as %s"
                  (Music.Key.to_pretty_string key)
                  (NEString.to_string @@ Version.Structure.to_string default_structure);
                selected_by_dancelor
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
          | No_content -> div [Alert.make ~level: Info [txt "This version does not have any content. This is usually because it has not been added yet. If you have access to the source of this precise version, consider sending it to an administrator."]]
          | Monolithic _ -> Components.Version_snippets.make version
          | Destructured {default_structure; _} -> Components.Version_snippets.make version ~params: (Model.Version_parameters.make ~structure: default_structure ());
        )];
      );
      div (
        match Model.Tune.other_names' tune with
        | [] -> []
        | [other_name] -> [section ~a: [a_class ["mt-2"]] [txtf "Also known as %s." (NEString.to_string other_name)]]
        | other_names ->
          [
            section ~a: [a_class ["mt-2"]] [
              txt "Also known as:";
              ul (List.map (li % List.singleton % txt % NEString.to_string) other_names);
            ];
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
              | [source_group] -> lwt [section ~a: [a_class ["mt-2"]] [txt "This specific version appears in "; show_source_group source_group]]
              | source_groups ->
                lwt [
                  section ~a: [a_class ["mt-2"]] [
                    txt "This specific version appears:";
                    ul (
                      List.map
                        (fun source_group -> li [txt "in "; show_source_group source_group])
                        source_groups
                    );
                  ];
                ]
          )
      );
      quick_explorer_links @@
        List.filter_map Fun.id [
          Option.flip_map version (fun version -> ("sets containing this version", lwt @@ Filter.(Any.set' % Set.versions' % Formula_list.exists' % Version.is') version));
          Some ("sets containing this tune", Filter.(Any.set' % Set.versions' % Formula_list.exists' % Version.tuneis') <$> lwt tune);
          Option.flip_map version (fun version -> ("books containing this version", lwt @@ Filter.(Any.book' % Book.versions_deep' % Formula_list.exists' % Version.is') version));
          Some ("books containing this tune", Filter.(Any.book' % Book.versions_deep' % Formula_list.exists' % Version.tuneis') <$> lwt tune);
        ];
      div [
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
      div [
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
