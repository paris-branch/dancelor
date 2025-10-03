open Nes
open Common
open Model
open Html

let show_lilypond_dialog version =
  let content_promise =
    let%lwt content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id version) in
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
        Utils.Button.close' ~return ();
        Utils.Button.make
          ~label: "Copy to clipboard"
          ~icon: "clipboard"
          ~classes: ["btn-primary"]
          ~onclick: (fun _ ->
            let%lwt content = content_promise in
            Utils.write_to_clipboard content;
            Utils.Toast.open_ ~title: "Copied to clipboard" [txt "The LilyPond content was copied to your clipboard."];
            return (some ());
            lwt_unit
          )
          ()
      ]

(* helpers to record all the changes that will be performed and spit them back
   to the user afterwards *)
let make_change_trackers () =
  let changes = ref [] in
  (
    (fun () -> List.rev_map fst !changes),
    (fun () -> List.rev_map snd !changes),
    (fun ?(action = fun () -> lwt_unit) html ->
      changes := (action, html) :: !changes
    )
  )

(* /!\ deduplicate this version INTO the other version *)
let deduplicate_confirmation_dialog ~this_version ~other_version =
  let (get_changes_actions, get_changes_html, add_changes) = make_change_trackers () in

  (* changes to other version *)
  let%lwt () =
    let (_, get_other_version_changes, add_other_version_changes) =
      make_change_trackers ()
    in
    (* tune *)
    let%lwt this_tune = Model.Version.tune' this_version in
    let%lwt other_tune = Model.Version.tune' other_version in
    if not (Entry.equal' this_tune other_tune) then
      failwith "Version de-duplicator: these two versions do not share the same tune.";
    (* key *)
    let this_key = Model.Version.key' this_version in
    let other_key = Model.Version.key' other_version in
    if this_key <> other_key then
      failwith "Version de-duplicator: these two versions do not share the same key.";
    (* FIXME: can do better? *)
    (* sources *)
    let%lwt this_sources = Model.Version.sources' this_version in
    let%lwt other_sources = Model.Version.sources' other_version in
    let other_sources = other_sources @ this_sources in
    (* FIXME: de-duplicate and detect the changes? *)
    (
      match this_sources with
      | [] -> ()
      | _ ->
        add_other_version_changes [
          txt "add the following sources:";
          ul (
            List.map
              (fun (source, structure) ->
                li [Formatters.Source.name' source; txt " ("; txt (NEString.to_string @@ Version.Content.structure_to_string structure); txt ")"]
              )
              this_sources
          );
        ]
    );
    (* arrangers *)
    let%lwt this_arrangers = Model.Version.arrangers' this_version in
    let%lwt other_arrangers = Model.Version.arrangers' other_version in
    if this_arrangers <> other_arrangers then
      failwith "Version de-duplicator: these two versions do not share the same arrangers.";
    (* FIXME: can do better? *)
    (* remark *)
    let this_remark = Model.Version.remark' this_version in
    let other_remark = Model.Version.remark' other_version in
    if this_remark <> other_remark then
      failwith "Version de-duplicator: these two versions do not share the same remark.";
    (* FIXME: can do better? *)
    (* disambiguation *)
    let this_disambiguation = Model.Version.disambiguation' this_version in
    let other_disambiguation = Model.Version.disambiguation' other_version in
    if this_disambiguation <> other_disambiguation then
      failwith "Version de-duplicator: these two versions do not share the same disambiguation.";
    (* FIXME: can do better? *)
    (* content *)
    let%lwt other_content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id other_version) in
    add_other_version_changes [txt "use its content; the content of the current version will be lost entirely."];
    (* that's it for changes to the other version; bundle them together as a change *)
    let other_version_formatted =
      span [
        Formatters.Version.name_disambiguation_and_sources' other_version;
        txt " [";
        Formatters.Version.id' other_version;
        txt "]"
      ]
    in
    (
      match get_other_version_changes () with
      | [] ->
        add_changes [txt "This will not change anything to the other version, "; other_version_formatted; txt "."];
      | changes ->
        add_changes
          ~action: (fun () ->
            (* FIXME: it would in fact be better if [make] didn't have any
               optional arguments. This would ensure that we cannot forget
               things. We use it so rarely anyway that the convenience isn't
               really anything we care about. *)
            ignore
            <$> Madge_client.call_exn
                Endpoints.Api.(route @@ Version Update)
                (Entry.id other_version) @@
                Model.Version.make
                  ~tune: other_tune
                  ~key: other_key
                  ~sources: other_sources
                  ~arrangers: other_arrangers
                  ~remark: other_remark
                  ~disambiguation: other_disambiguation
                  ~content: other_content
                  ()
          (* FIXME: we should report nicely if things fail *)
          )
          [
            txt "update the other version, ";
            other_version_formatted;
            txt ", in the following way:";
            ul (List.map li changes);
          ]
    );
    lwt_unit
  in

  (* how to update a version and its params from a set or a book *)
  let replace_version_and_params a_version a_version_params =
    if Entry.equal' a_version this_version then
      (
        other_version,
        Model.VersionParameters.compose
          (
            (* if [this_version] is monolithic, then it comes
               with a specific structure, and this information
               was used when making the set, so we keep it as a
               version parameter to the updated set *)
            match Model.Version.content' this_version with
            | Monolithic {structure; _} -> Model.VersionParameters.make ~structure ()
            | _ -> Model.VersionParameters.none
          )
          a_version_params (* existing parameters take precedence *)
      )
    else (a_version, a_version_params)
  in

  (* changes to sets *)
  let%lwt sets =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Set Search)
        Slice.everything
        (Filter.(Set.memversion') this_version)
  in
  List.iter
    (fun set ->
      add_changes
        ~action: (fun () ->
          let%lwt contents = Model.Set.contents' set in
          let contents = List.map (uncurry replace_version_and_params) contents in
          ignore
          <$> Madge_client.call_exn
              Endpoints.Api.(route @@ Set Update)
              (Entry.id set) @@
              Model.Set.set_contents contents (Entry.value set)
        )
        [txt "replace the version in set "; Formatters.Set.name' set; txt "."]
    )
    sets;

  (* changes to books *)
  let%lwt books =
    snd
    <$> Madge_client.call_exn
        Endpoints.Api.(route @@ Book Search)
        Slice.everything
        (Filter.(Book.memversion') this_version)
  in
  List.iter
    (fun book ->
      add_changes
        ~action: (fun () ->
          let%lwt contents = Model.Book.contents' book in
          let contents =
            List.map
              (function
                | Model.Book.Dance (dance, DanceVersion (a_version, a_version_params)) ->
                  Model.Book.Dance (dance, uncurry Model.Book.danceversion (replace_version_and_params a_version a_version_params))
                | Model.Book.Version (a_version, a_version_params) ->
                  uncurry Model.Book.version (replace_version_and_params a_version a_version_params)
                | page -> page
              )
              contents
          in
          ignore
          <$> Madge_client.call_exn
              Endpoints.Api.(route @@ Book Update)
              (Entry.id book) @@
              Model.Book.set_contents contents (Entry.value book)
        )
        [
          txt
            "replace the version in book ";
          Formatters.Book.title' book;
          txt "."
        ]
    )
    books;

  (* removal of the current version *)
  add_changes
    ~action: (fun () ->
      ignore
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Delete) (Entry.id this_version)
    )
    [
      txt "delete the current version, ";
      Formatters.Version.name_disambiguation_and_sources' this_version;
      txt " [";
      Formatters.Version.id' this_version;
      txt "]";
      txt ".";
    ];

  (* report *)
  let%lwt user_input =
    Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "De-duplicate a version")
      [Utils.Alert.make ~level: Warning [txt "This action is irreversible."];
      div ~a: [a_class ["mt-4"]] [txt "This will:"; ul (List.map li (get_changes_html ()))]]
      ~buttons: [
        Utils.Button.cancel' ~return ();
        Utils.Button.make
          ~classes: ["btn-primary"]
          ~label: "Proceed"
          ~label_processing: "Proceeding"
          ~icon: "node-minus"
          ~onclick: (lwt % return % some)
          ();
      ]
  in
  (* let's go *)
  match user_input with
  | None -> lwt_unit
  | Some() ->
    Utils.Toast.open_
      ~title: "De-duplicating a version"
      [
        txt
          "Dancelor has started de-duplicating this version. Hopefully, you see \
           no error, and another toast comes to announce the good news! \
           Otherwise, please report immediately to a system administrator, \
           because the database might be in an odd state.";
      ];
    Lwt_list.iter_s (fun action -> action ()) (get_changes_actions ());%lwt
    Utils.Toast.open_
      ~type_: Forever
      ~title: "De-duplicated a version"
      [txt
        "The version has been de-duplicated successfully! This means that \
           you are on a page that does not exist anymore. Run, you fools!"]
      ~buttons: [
        Utils.Button.make_a
          ~label: "Go to other version"
          ~icon: "music-note-beamed"
          ~classes: ["btn-primary"]
          ~href: (S.const @@ Endpoints.Page.href_version (Entry.id other_version))
          ();
      ];
    lwt_unit

let deduplication_dialog ~version ~other_versions_promise () =
  let%lwt other_versions = other_versions_promise in
  ignore
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "De-duplicate a version")
      [txt
        "Only do this if the two versions are actually the same, or if the other \
         one is a destructured version that can encompass this one.";
      Tables.versions
        other_versions
        ~onclick: (fun other_version ->
          deduplicate_confirmation_dialog ~this_version: version ~other_version;%lwt
          return (some ());
          lwt_unit
        );
      ]
      ~buttons: [
        Utils.Button.close' ~return ();
      ]

let create ?context id =
  MainPage.madge_call_or_404 (Version Get) id @@ fun version ->
  let%lwt tune = Model.Version.tune' version in
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
      Components.ContextLinks.make_and_render
        ?context
        ~this_page: (Endpoints.Page.href_version id)
        (lwt @@ Any.version version);
    ]
    ~title: (NEString.to_string <$> Version.one_name' version)
    ~subtitles: [
      Formatters.Version.tune_description' version;
      Formatters.Version.description' ~arranger_links: true version;
    ]
    ~share: (Version version)
    ~actions: (
      Lwt.l2
        (@)
        (
          lwt
            [
              Utils.Button.make
                ~label: "Download PDF"
                ~icon: "file-pdf"
                ~classes: ["dropdown-item"]
                ~onclick: (fun _ -> ignore <$> VersionDownloadDialog.create_and_open version)
                ();
              Utils.Button.make
                ~classes: ["dropdown-item"]
                ~label: "Show LilyPond"
                ~label_processing: "Showing LilyPond..."
                ~icon: "file-music"
                ~onclick: (fun () -> show_lilypond_dialog version)
                ();
              Utils.Button.make
                ~label: "Add to current set"
                ~icon: "plus-square"
                ~classes: ["dropdown-item"]
                ~onclick: (fun _ ->
                  SetEditor.add_to_storage id;%lwt
                  Utils.Toast.open_
                    ~title: "Added to current set"
                    [
                      txt "This version was added to the current set. You can see that in the ";
                      a ~a: [a_href Endpoints.Page.(href SetAdd)] [txt "set editor"];
                      txt ".";
                    ];
                  lwt_unit
                )
                ();
              Utils.Button.make
                ~label: "Add to current book"
                ~icon: "plus-square"
                ~classes: ["dropdown-item"]
                ~onclick: (fun _ ->
                  BookEditor.add_version_to_storage id;%lwt
                  Utils.Toast.open_
                    ~title: "Added to current book"
                    [
                      txt "This version was added to the current book. You can see that in the ";
                      a ~a: [a_href Endpoints.Page.(href BookAdd)] [txt "book editor"];
                      txt ".";
                    ];
                  lwt_unit
                )
                ();
              Utils.Button.make_a
                ~classes: ["dropdown-item"]
                ~href: (S.const @@ Endpoints.Page.(href VersionEdit) id)
                ~icon: "pencil-square"
                ~label: "Edit"
                ();
              Utils.Button.make_a
                ~classes: ["dropdown-item"]
                ~href: (S.const @@ Endpoints.Page.(href TuneEdit) (Entry.id tune))
                ~icon: "pencil-square"
                ~label: "Edit tune"
                ();
              Utils.Action.delete
                ~model: "version"
                ~onclick: (fun () -> Madge_client.call Endpoints.Api.(route @@ Version Delete) (Entry.id version))
                ();
              Utils.Button.make
                ~classes: ["dropdown-item"; "btn-warning"]
                ~label: "De-duplicate"
                ~icon: "node-minus"
                ~onclick: (deduplication_dialog ~version ~other_versions_promise)
                ()
            ]
        )
        (
          match%lwt Tune.scddb_id' <$> Model.Version.tune' version with
          | None -> lwt_nil
          | Some scddb_id ->
            lwt
              [
                Utils.Button.make_a
                  ~classes: ["dropdown-item"]
                  ~href: (S.const @@ Uri.to_string @@ SCDDB.tune_uri scddb_id)
                  ~icon: "box-arrow-up-right"
                  ~label: "See on SCDDB"
                  ()
              ]
        )
    )
    [
      div
        [
          with_span_placeholder @@
            match%lwt Tune.date' <$> Model.Version.tune' version with
            | None -> lwt_nil
            | Some date ->
              lwt [txt "Composed "; txt (PartialDate.to_pretty_string ~at: true date); txt "."]
        ];
      (
        (* For de-structured versions, show one of the common structures. *)
        match Model.Version.content' version with
        | Monolithic _ -> Components.VersionSnippets.make version
        | Destructured {common_structures; _} ->
          let structure = NEList.hd common_structures in
          div [
            txt ("Shown here as " ^ NEString.to_string (Version.Content.structure_to_string structure));
            (
              match NEList.tl common_structures with
              | [] -> txt "."
              | other_structures ->
                span (
                  [txt ", but is commonly also seen as "] @
                  List.interspersei
                    (fun _ -> txt ", ")
                    ~last: (fun _ -> txt " or ")
                    (List.map (txt % NEString.to_string % Model.Version.Content.structure_to_string) other_structures) @
                    [txt "."]
                )
            );
            Components.VersionSnippets.make
              version
              ~params: (Model.VersionParameters.make ~structure ());
          ]
      );
      R.div (
        S.from' [] @@
          match%lwt Model.Version.other_names' version with
          | [] -> lwt_nil
          | other_names ->
            lwt [
              txt "Also known as:";
              ul (List.map (li % List.singleton % txt % NEString.to_string) other_names);
            ]
      );
      R.div (
        S.from' [] @@
          match%lwt Model.Version.sources' version with
          | [] -> lwt_nil
          | sources ->
            lwt [
              txt "Appears:";
              ul (
                List.map
                  (fun (source, structure) ->
                    li [
                      txt "in ";
                      Formatters.Source.name' source;
                      txt " as ";
                      txt (NEString.to_string @@ Model.Version.Content.structure_to_string structure);
                    ]
                  )
                  sources
              )
            ]
      );
      Utils.quick_explorer_links
        [
          ("sets containing this version", lwt @@ Filter.(Any.set' % Set.memversion') version);
          ("sets containing this tune", Filter.(Any.set' % Set.existsversion' % Version.tuneis') <$> Model.Version.tune' version);
          ("books containing this version", lwt @@ Filter.(Any.book' % Book.memversiondeep') version);
          ("books containing this tune", Filter.(Any.book' % Book.existsversiondeep' % Version.tuneis') <$> Model.Version.tune' version);
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
