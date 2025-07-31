open Nes
open Common

open Components
open Html
open Utils

type ('tune, 'bars, 'key, 'structure, 'arrangers, 'remark, 'sources, 'disambiguation, 'content) gen = {
  tune: 'tune;
  bars: 'bars;
  key: 'key;
  structure: 'structure;
  arrangers: 'arrangers;
  remark: 'remark;
  sources: 'sources;
  disambiguation: 'disambiguation;
  content: 'content;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type tune = Model.Tune.t
  let tune_to_yojson _ = assert false
  let tune_of_yojson _ = assert false
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false
  type source = Model.Source.t
  let source_to_yojson _ = assert false
  let source_of_yojson _ = assert false

  type t =
  (tune Entry.Id.t list, string, string, string, person Entry.Id.t list, string, source Entry.Id.t list, string, string) gen
  [@@deriving yojson]

  let empty = {
    tune = [];
    bars = "";
    key = "";
    structure = "";
    arrangers = [];
    remark = "";
    sources = [];
    disambiguation = "";
    content = ""
  }
end

module Editor = struct
  type t = {
    elements:
    ((Selector.one, Model.Tune.t) Selector.t, int Input.Text.t, Music.key Input.Text.t, string Input.Text.t, (Selector.many, Model.Person.t) Selector.t, string Input.Text.t, (Selector.many, Model.Source.t) Selector.t, string Input.Text.t, string Input.Text.t) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Selector.raw_signal editor.elements.tune) @@ fun tune ->
    S.bind (Input.Text.raw_signal editor.elements.bars) @@ fun bars ->
    S.bind (Input.Text.raw_signal editor.elements.key) @@ fun key ->
    S.bind (Input.Text.raw_signal editor.elements.structure) @@ fun structure ->
    S.bind (Selector.raw_signal editor.elements.arrangers) @@ fun arrangers ->
    S.bind (Input.Text.raw_signal editor.elements.remark) @@ fun remark ->
    S.bind (Selector.raw_signal editor.elements.sources) @@ fun sources ->
    S.bind (Input.Text.raw_signal editor.elements.disambiguation) @@ fun disambiguation ->
    S.bind (Input.Text.raw_signal editor.elements.content) @@ fun content ->
    S.const {tune; bars; key; structure; arrangers; remark; sources; disambiguation; content}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Selector.signal_one editor.elements.tune) @@ fun tune ->
    RS.bind (Input.Text.signal editor.elements.bars) @@ fun bars ->
    RS.bind (Input.Text.signal editor.elements.key) @@ fun key ->
    RS.bind (Input.Text.signal editor.elements.structure) @@ fun structure ->
    RS.bind (Selector.signal_many editor.elements.arrangers) @@ fun arrangers ->
    RS.bind (Input.Text.signal editor.elements.remark) @@ fun remark ->
    RS.bind (Selector.signal_many editor.elements.sources) @@ fun sources ->
    RS.bind (Input.Text.signal editor.elements.disambiguation) @@ fun disambiguation ->
    RS.bind (Input.Text.signal editor.elements.content) @@ fun content ->
    RS.pure {tune; bars; key; structure; arrangers; remark; sources; disambiguation; content}

  let with_or_without_local_storage ~text ~tune f =
    match text, tune with
    | (None, None) ->
      lwt @@ Cutils.with_local_storage "VersionEditor" (module RawState) raw_state f
    | _ ->
      lwt @@ f {RawState.empty with tune = Option.value ~default: [] tune}

  let create ~text ~tune : t Lwt.t =
    with_or_without_local_storage ~text ~tune @@ fun initial_state ->
    let tune =
      Selector.make
        ~arity: Selector.one
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Tune.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Tune Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Tune.get
        initial_state.tune
    in
    let bars =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.bars
        ~label: "Number of bars"
        ~placeholder: "eg. 32 or 48"
        ~validator: (Option.to_result ~none: "The number of bars has to be an integer." % int_of_string_opt)
        ()
    in
    let key =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.key
        ~label: "Key"
        ~placeholder: "eg. A or F#m"
        ~validator: (Option.to_result ~none: "Enter a valid key, eg. A of F#m." % Music.key_of_string_opt)
        ()
    in
    let structure =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.structure
        ~label: "Structure"
        ~placeholder: "eg. AABB or ABAB"
        ~validator: ok
        ()
    in
    let arrangers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Person.get
        initial_state.arrangers
    in
    let remark =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.remark
        ~label: "Remark"
        ~placeholder: "Any additional information that doesn't fit in the other fields."
        ~validator: ok
        ()
    in
    let sources =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Source.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Source Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Source.get
        initial_state.sources
    in
    let disambiguation =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.disambiguation
        ~label: "Disambiguation"
        ~placeholder: "If there are multiple versions with the same name, this field must be used to distinguish them."
        ~validator: ok
        ()
    in
    let content =
      Input.Text.make
        ~type_: Textarea
        ~initial_value: initial_state.content
        ~label: "LilyPond content"
        ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>"
        ~validator: (Result.of_string_nonempty ~empty: "Cannot be empty.")
        ()
    in
    {
      elements = {tune; bars; key; structure; arrangers; remark; sources; disambiguation; content};
    }

  let clear (editor : t) =
    Selector.clear editor.elements.tune;
    Input.Text.clear editor.elements.bars;
    Input.Text.clear editor.elements.key;
    Input.Text.clear editor.elements.structure;
    Selector.clear editor.elements.arrangers;
    Input.Text.clear editor.elements.remark;
    Selector.clear editor.elements.sources;
    Input.Text.clear editor.elements.disambiguation;
    Input.Text.clear editor.elements.content

  let value (editor : t) =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {tune; bars; key; structure; arrangers; remark; sources; disambiguation; content} ->
      lwt_some @@
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
end

let create ?on_save ?text ?tune () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text ~tune in
  Page.make'
    ~title: (lwt "Add a version")
    [Selector.render
      ~make_result: AnyResult.make_tune_result'
      ~field_name: "Tune"
      ~model_name: "tune"
      ~create_dialog_content: (fun ?on_save text -> TuneEditor.create ?on_save ~text ())
      editor.elements.tune;
    Input.Text.html editor.elements.bars;
    Input.Text.html editor.elements.key;
    Input.Text.html editor.elements.structure;
    Selector.render
      ~make_result: AnyResult.make_person_result'
      ~field_name: "Arrangers"
      ~model_name: "person"
      ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
      editor.elements.arrangers;
    Selector.render
      ~make_result: AnyResult.make_source_result'
      ~field_name: "Sources"
      ~model_name: "source"
      ~create_dialog_content: (fun ?on_save text -> SourceEditor.create ?on_save ~text ())
      editor.elements.sources;
    Input.Text.html editor.elements.disambiguation;
    Input.Text.html editor.elements.remark;
    Input.Text.html editor.elements.content;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          match%lwt Editor.value editor with
          | None -> lwt_unit
          | Some version ->
            ignore
            <$> Page.open_dialog' @@ fun return ->
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
                  Button.save
                    ~onclick: (fun () ->
                      let%lwt version =
                        Madge_client.call_exn
                          Endpoints.Api.(route @@ Version Create)
                          version
                      in
                      Editor.clear editor;
                      (
                        match on_save with
                        | None ->
                          Components.Toast.open_
                            ~title: "Version created"
                            [txt "The version ";
                            Formatters.Version.name' ~link: true version;
                            txt " has been created successfully."]
                            ~buttons: [
                              Components.Button.make_a
                                ~label: "Go to version"
                                ~classes: ["btn-primary"]
                                ~href: (S.const @@ Endpoints.Page.href_version @@ Entry.id version)
                                ();
                            ]
                        | Some on_save -> on_save version
                      );
                      lwt_unit
                    )
                    ();
                ]
        )
        ();
    ]
