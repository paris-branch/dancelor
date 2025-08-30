open Nes
open Common
open Model
open Html

let show_lilypond_dialog id =
  let content_promise = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) id in
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

let create ?context id =
  MainPage.madge_call_or_404 (Version Get) id @@ fun version ->
  let%lwt tune = Model.Version.tune' version in
  let other_versions_lwt =
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
      Formatters.Version.tune_aka' version;
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
                ~onclick: (fun () -> show_lilypond_dialog id)
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
      div ~a: [a_class ["text-center"]] [Components.VersionSvg.make version];
      div ~a: [a_class ["d-flex"; "justify-content-end"]] [Components.VersionOgg.make version];
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
                let%lwt other_versions = other_versions_lwt in
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
