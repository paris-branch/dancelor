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
    ~create_dialog_content: TuneEditor.create
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
        ~create_dialog_content: PersonEditor.create
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
        ~create_dialog_content: SourceEditor.create
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
  (
    let open Plus.TupleElt in
    Plus.prepare
      ~label: "Content"
      ~cast: (function
        | Zero content -> Model.Version.Content.Full content
        | Succ Zero parts ->
          Model.Version.Content.Parts (
            List.map
              (Pair.map_snd (fun (melody, chords) -> Model.Version.Content.{melody; chords}))
              parts
          )
        | _ -> assert false (* types guarantee this is not reachable *)
      )
      ~uncast: (function
        | Model.Version.Content.Full content -> Zero content
        | Model.Version.Content.Parts parts ->
          one (
            List.map
              (Pair.map_snd (fun Model.Version.Content.{melody; chords} -> (melody, chords)))
              parts
          )
      )
      (
        let open Plus.Bundle in
        Input.prepare
          ~type_: (Textarea {rows = 20})
          ~font: Monospace
          ~label: "Full"
          ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>"
          ~serialise: id
          ~validate: (S.const % Result.of_string_nonempty ~empty: "Cannot be empty.")
          ~template: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\major\n    \\time 4/4\n\n    %% add tune here\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      %% add chords here\n    }\n  }\n>>"
          () ^::
        Star.prepare
          ~label: "Parts"
          (
            Cpair.prepare
              ~label: "Part"
              (
                Input.prepare
                  ~type_: Text
                  ~label: "Part name"
                  ~placeholder: "eg. A, B, C"
                  ~serialise: Char.to_string
                  ~validate: (S.const % Option.to_result ~none: "Must be one character" % Char.of_string_opt)
                  ()
              )
              (
                Cpair.prepare
                  ~label: "Part content"
                  (
                    Input.prepare
                      ~type_: (Textarea {rows = 13})
                      ~label: "Melody"
                      ~serialise: id
                      ~validate: (S.const % ok)
                      ~placeholder: "\\relative f' {\n  \\partial 4 a4 |\n  d,4 fis8 a b4 a |\n  b8 a b cis d4 d8 cis |\n  b4 d8 fis b a g fis |\n  e d cis b a g fis e |\n  \\break\n\n  d4 fis8 a b4 a |\n  b8 a b cis d4 d8 cis |\n  b4 d8 fis b a g fis |\n  e d e fis d4\n}"
                      ~template: "\\relative f' {\n  %% add part's melody here\n}\n"
                      ()
                  )
                  (
                    Input.prepare
                      ~type_: (Textarea {rows = 2})
                      ~label: "Chords"
                      ~serialise: id
                      ~validate: (S.const % ok)
                      ~placeholder: "s4 | d2 g | a d | b:m e:m | a2 a:7 |\nd2 g | a d | b:m e:m | a2:7 d4"
                      ()
                  )
              )
          ) ^::
        nil
      )
  ) ^::
  nil

let assemble (tune, (bars, (key, (structure, (arrangers, (remark, (sources, (disambiguation, (content, ()))))))))) =
  Model.Version.make ~tune ~bars ~key ~structure ~arrangers ~remark ~sources ~disambiguation ~content ()

let preview version =
  Option.fold ~none: false ~some: (const true)
  <$> Page.open_dialog @@ fun return ->
    Page.make'
      ~title: (lwt "Preview")
      [Components.VersionSnippets.make_preview ~show_logs: true version]
      ~buttons: [
        Button.cancel' ~return ();
        Button.save ~onclick: (fun () -> return (Some ()); lwt_unit) ();
      ]

let submit mode version =
  match mode with
  | Editor.Edit prev_version -> Madge_client.call_exn Endpoints.Api.(route @@ Version Update) (Entry.id prev_version) version
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Version Create) version

let unsubmit version =
  let%lwt content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id version) in
  lwt @@ Model.Version.set_content (Some content) (Entry.value version)

let disassemble version =
  let%lwt tune = Model.Version.tune version in
  let bars = Model.Version.bars version in
  let key = Model.Version.key version in
  let structure = Model.Version.structure version in
  let%lwt arrangers = Model.Version.arrangers version in
  let remark = Model.Version.remark version in
  let%lwt sources = Model.Version.sources version in
  let disambiguation = Model.Version.disambiguation version in
  let content = Model.Version.content version in
  lwt (tune, (bars, (key, (structure, (arrangers, (remark, (sources, (disambiguation, (content, ())))))))))

(* FIXME: There used to be a way to start a version editor with a tune already
   selected and we lost it. It is only marginally important, but it would be
   nice to bring it back. *)

let create mode =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "version"
    ~icon: "music-note-beamed"
    editor
    ~mode
    ~href: (Endpoints.Page.href_version % Entry.id)
    ~format: (Formatters.Version.name' ~link: true)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~preview
    ~check_product: Model.Version.equal
