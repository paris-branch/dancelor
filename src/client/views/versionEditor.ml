open Nes
open Common

open Js_of_ocaml
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
  (* Dirty trick to convince Yojson to serialise slugs. *)
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
  (tune Slug.t list, string, string, string, person Slug.t list, string, source Slug.t list, string, string) gen
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
      Lwt.return @@ Cutils.with_local_storage "VersionEditor" (module RawState) raw_state f
    | _ ->
      Lwt.return @@ f {RawState.empty with tune = Option.value ~default: [] tune}

  let create ~text ~tune : t Lwt.t =
    with_or_without_local_storage ~text ~tune @@ fun initial_state ->
    let tune =
      Selector.make
        ~arity: Selector.one
        ~search: (fun slice input ->
          let%rlwt filter = Lwt.return (Model.Tune.Filter.from_string input) in
          Lwt.map Result.ok @@
            Madge_cohttp_lwt_client.call
              Endpoints.Api.(route @@ Tune Search)
              slice
              filter
        )
        ~serialise: Entry.slug
        ~unserialise: Model.Tune.get
        initial_state.tune
    in
    let bars =
      Input.Text.make initial_state.bars @@
        Option.to_result ~none: "The number of bars has to be an integer." % int_of_string_opt
    in
    let key =
      Input.Text.make initial_state.key @@
        Option.to_result ~none: "Enter a valid key, eg. A of F#m." % Music.key_of_string_opt
    in
    let structure = Input.Text.make initial_state.structure @@ Result.ok in
    let arrangers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
          Lwt.map Result.ok @@
            Madge_cohttp_lwt_client.call
              Endpoints.Api.(route @@ Person Search)
              slice
              filter
        )
        ~serialise: Entry.slug
        ~unserialise: Model.Person.get
        initial_state.arrangers
    in
    let remark = Input.Text.make initial_state.remark @@ Result.ok in
    let sources =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = Lwt.return (Model.Source.Filter.from_string input) in
          Lwt.map Result.ok @@
            Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Source Search) slice filter
        )
        ~serialise: Entry.slug
        ~unserialise: Model.Source.get
        initial_state.sources
    in
    let disambiguation = Input.Text.make initial_state.disambiguation @@ Result.ok in
    let content =
      Input.Text.make initial_state.content @@
        Result.of_string_nonempty ~empty: "Cannot be empty."
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
    | None -> Lwt.return_none
    | Some {tune; bars; key; structure; arrangers; remark; sources; disambiguation; content} ->
      Lwt.return_some @@
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
  let title = "Add a version" in
  let editor = Editor.create ~text ~tune in
  Page.make
    ~title: (S.const title)
    [L.div
      (
        let%lwt editor = editor in
        Lwt.return
          [
            Selector.render
              ~make_result: AnyResult.make_tune_result'
              ~field_name: "Tune"
              ~model_name: "tune"
              ~create_dialog_content: (fun ?on_save text -> TuneEditor.create ?on_save ~text ())
              editor.elements.tune;
            Input.Text.render
              editor.elements.bars
              ~label: "Number of bars"
              ~placeholder: "eg. 32 or 48";
            Input.Text.render
              editor.elements.key
              ~label: "Key"
              ~placeholder: "eg. A or F#m";
            Input.Text.render
              editor.elements.structure
              ~label: "Structure"
              ~placeholder: "eg. AABB or ABAB";
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
            Input.Text.render
              editor.elements.disambiguation
              ~label: "Disambiguation"
              ~placeholder: "If there are multiple versions with the same name, this field must be used to distinguish them.";
            Input.Text.render
              editor.elements.remark
              ~label: "Remark"
              ~placeholder: "Any additional information that doesn't fit in the other fields.";
            Input.Text.render_as_textarea
              editor.elements.content
              ~label: "LilyPond content"
              ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>";
          ]
      )]
    ~buttons: [
      L.div
        (
          let%lwt editor = editor in
          Lwt.return
            [
              Button.clear
                ~onclick: (fun () -> Editor.clear editor)
                ();
              Button.save
                ~disabled: (S.map Option.is_none (Editor.state editor))
                ~onclick: (fun () ->
                  match%lwt Editor.value editor with
                  | None -> Lwt.return_unit
                  | Some version ->
                    Lwt.map ignore @@
                    Page.open_dialog' @@ fun return ->
                    Page.make
                      ~title: (S.const "Preview")
                      [div [VersionSvg.make_preview version];
                      div
                        [
                          audio
                            ~a: [a_controls ()]
                            ~src: (Endpoints.Api.(href @@ Version PreviewOgg) Model.VersionParameters.none version)
                            [];
                        ];
                      ]
                      ~buttons: [
                        Button.cancel' ~return ();
                        Button.save
                          ~onclick: (fun () ->
                            let%lwt version =
                              Madge_cohttp_lwt_client.call
                                Endpoints.Api.(route @@ Version Create)
                                version
                            in
                            Editor.clear editor;
                            (
                              match on_save with
                              | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_version (Entry.slug version))
                              | Some on_save -> on_save version
                            );
                            Lwt.return_unit
                          )
                          ();
                      ]
                )
                ();
            ]
        )
    ]
