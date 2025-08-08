open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Selector.prepare
    ~make_result: AnyResult.make_tune_result'
    ~label: "Tune"
    ~model_name: "tune"
    ~create_dialog_content: (fun ?on_save text -> TuneEditor.create ?on_save ~text ())
    ~search: (fun slice input ->
      let%rlwt filter = lwt (Filter.Tune.from_string input) in
      ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Tune Search) slice filter
    )
    ~unserialise: Model.Tune.get
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Number of bars"
    ~placeholder: "eg. 32 or 48"
    ~serialise: string_of_int
    ~validate: (
      S.const %
        Option.to_result ~none: "The number of bars has to be an integer." %
        int_of_string_opt
    )
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Key"
    ~placeholder: "eg. A or F#m"
    ~serialise: Music.key_to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid key, eg. A of F#m." %
        Music.key_of_string_opt
    )
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Structure"
    ~placeholder: "eg. AABB or ABAB"
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Star.prepare
    ~label: "Arrangers"
    (
      Selector.prepare
        ~make_result: AnyResult.make_person_result'
        ~label: "Arranger"
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~unserialise: Model.Person.get
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Remark"
    ~placeholder: "Any additional information that doesn't fit in the other fields."
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Star.prepare
    ~label: "Sources"
    (
      Selector.prepare
        ~make_result: AnyResult.make_source_result'
        ~label: "Source"
        ~model_name: "source"
        ~create_dialog_content: (fun ?on_save text -> SourceEditor.create ?on_save ~text ())
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Source.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Source Search) slice filter
        )
        ~unserialise: Model.Source.get
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Disambiguation"
    ~placeholder: "If there are multiple versions with the same name, this field must be used to distinguish them."
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Input.prepare
    ~type_: Textarea
    ~label: "LilyPond content"
    ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "Cannot be empty.")
    () ^::
  nil

let preview (tune, (bars, (key, (structure, (arrangers, (remark, (sources, (disambiguation, (content, ()))))))))) : Model.Version.t option Lwt.t =
  let version =
    Model.Version.make
      ~tune
      ~bars
      ~key
      ~structure
      ~arrangers
      ~remark
      ~sources
      ~disambiguation
      ~content
      ()
  in
  Page.open_dialog @@ fun return ->
  Page.make'
    ~title: (lwt "Preview")
    [div [VersionSvg.make_preview version];
    div
      [
        audio
          ~a: [a_controls ()]
          ~src: (Endpoints.Api.(href @@ Version PreviewOgg) version Model.VersionParameters.none RenderingParameters.none)
          [];
      ];
    ]
    ~buttons: [
      Button.cancel' ~return ();
      Button.save ~onclick: (fun () -> return (some version); lwt_unit) ();
    ]

let submit = Madge_client.call_exn Endpoints.Api.(route @@ Version Create)

(* FIXME: A way to add a button to the textarea component, so as to bring back
   the glue content in a better way. *)
(* Button.make *)
(*   ~label: "Add glue content" *)
(*   ~classes: ["btn-secondary"] *)
(*   ~disabled: (S.map ((<>) "") (Component.raw_signal editor.elements.content)) *)
(*   ~onclick: (fun () -> Component.set editor.elements.content "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\major\n    \\time 4/4\n\n    %% add tune here\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      %% add chords here\n    }\n  }\n>>"; lwt_unit) *)
(*   (); *)

(* FIXME: There used to be a way to start a version editor with a tune already
   selected and we lost it. It is only marginally important, but it would be
   nice to bring it back. *)

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "version"
    ~icon: "music-note-beamed"
    editor
    ?on_save
    ?initial_text: text
    ~href: (Endpoints.Page.href_version % Entry.id)
    ~format: (Formatters.Version.name' ~link: true)
    ~preview
    ~submit
