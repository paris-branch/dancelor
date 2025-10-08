open Nes
open Common

open Components
open Html
open Utils

let bars =
  Input.prepare
    ~type_: Text
    ~label: "Number of bars"
    ~serialise: string_of_int
    ~validate: (
      S.const %
        Option.to_result ~none: "The number of bars has to be an integer." %
        int_of_string_opt
    )

let structure =
  Input.prepare
    ~type_: Text
    ~label: "Structure"
    ~placeholder: "eg. AABB or ABAB"
    ~serialise: (NEString.to_string % Model.Version.Content.structure_to_string)
    ~validate: (
      S.const % Option.to_result ~none: "not a valid structure" %
        (fun s -> Option.bind (NEString.of_string s) Model.Version.Content.structure_of_string
        )
    )

let content_full () =
  Cpair.prepare
    ~label: "Monolithic"
    (
      Cpair.prepare
        ~label: "FIXME"
        (bars ~placeholder: "eg. 32 or 48" ())
        (structure ())
    )
    (
      Input.prepare
        ~type_: (Textarea {rows = 20})
        ~font: Monospace
        ~label: "Full LilyPond"
        ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>"
        ~serialise: id
        ~validate: (S.const % Result.of_string_nonempty ~empty: "Cannot be empty.")
        ~template: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\major\n    \\time 4/4\n\n    %% add tune here\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      %% add chords here\n    }\n  }\n>>"
        ()
    )

let content_in_parts () =
  Cpair.prepare
    ~label: "Destructured"
    (
      Star.prepare_non_empty
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
                ~label: "FIXME"
                (bars ~placeholder: "most often 8" ())
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
            )
        )
    )
    (
      Star.prepare_non_empty
        ~label: "Common structures"
        (structure ())
    )

let content () =
  let open Plus.TupleElt in
  Plus.prepare
    ~label: "Content"
    ~cast: (function
      | Zero ((bars, structure), lilypond) -> Model.Version.Content.Monolithic {bars; structure; lilypond}
      | Succ Zero (parts, common_structures) ->
        Model.Version.Content.Destructured
          {
            parts = (
              NEList.map
                (Pair.map_snd (fun (bars, (melody, chords)) -> Model.Version.Content.{bars; melody; chords}))
                parts
            );
            common_structures;
          }
      | _ -> assert false (* types guarantee this is not reachable *)
    )
    ~uncast: (function
      | Model.Version.Content.Monolithic {bars; structure; lilypond} -> Zero ((bars, structure), lilypond)
      | Model.Version.Content.Destructured {parts; common_structures} ->
        one (
          NEList.map
            (Pair.map_snd (fun Model.Version.Content.{bars; melody; chords} -> (bars, (melody, chords))))
            parts,
          common_structures
        )
    )
    (
      let open Plus.Bundle in
      content_full () ^::
      content_in_parts () ^::
      nil
    )

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
    ~label: "Key"
    ~placeholder: "eg. A or F#m"
    ~serialise: Music.Key.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid key, eg. A of F#m." %
        Music.Key.of_string_opt
    )
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
      Cpair.prepare
        ~label: "Source"
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
        )
        (structure ())
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Disambiguation"
    ~placeholder: "If there are multiple versions with the same name, this field must be used to distinguish them."
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  content () ^::
  nil

let assemble (tune, (key, (arrangers, (remark, (sources, (disambiguation, (content, ()))))))) =
  Model.Version.make ~tune ~key ~arrangers ~remark ~sources ~disambiguation ~content ()

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
  (* NOTE: The API erases the LilyPond from versions, so we need to pull the
     full content ourselves and re-insert it in the version. *)
  let%lwt content = Madge_client.call_exn Endpoints.Api.(route @@ Version Content) (Entry.id version) in
  lwt @@ Model.Version.set_content content (Entry.value version)

let disassemble version =
  let%lwt tune = Model.Version.tune version in
  let key = Model.Version.key version in
  let%lwt arrangers = Model.Version.arrangers version in
  let remark = Model.Version.remark version in
  let%lwt sources = Model.Version.sources version in
  let disambiguation = Model.Version.disambiguation version in
  let content = Model.Version.content version in
  lwt (tune, (key, (arrangers, (remark, (sources, (disambiguation, (content, ())))))))

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
