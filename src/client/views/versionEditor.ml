open Nes
open Js_of_ocaml
open Components
open Html
open Utils
module SCDDB = Dancelor_common.SCDDB
module Endpoints = Dancelor_common.Endpoints

type ('tune, 'bars, 'key, 'structure, 'arrangers, 'remark, 'disambiguation, 'content) gen = {
  tune: 'tune;
  bars: 'bars;
  key: 'key;
  structure: 'structure;
  arrangers: 'arrangers;
  remark: 'remark;
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

  type t =
    ((string * tune Slug.t list), string, string, string, (string * person Slug.t list), string, string, string) gen
  [@@deriving yojson]

  let empty = {
    tune = ("", []);
    bars = "";
    key = "";
    structure = "";
    arrangers = ("", []);
    remark = "";
    disambiguation = "";
    content = ""
  }
end

module Editor = struct
  type t = {
    elements:
      ((Selector.one, Model.Tune.t) Selector.t, int Input.Text.t, Dancelor_common.Music.key Input.Text.t, string Input.Text.t, (Selector.many, Model.Person.t) Selector.t, string Input.Text.t, string Input.Text.t, string Input.Text.t) gen;
    set_interacted: unit -> unit;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Selector.raw_signal editor.elements.tune) @@ fun tune ->
    S.bind (Input.Text.raw_signal editor.elements.bars) @@ fun bars ->
    S.bind (Input.Text.raw_signal editor.elements.key) @@ fun key ->
    S.bind (Input.Text.raw_signal editor.elements.structure) @@ fun structure ->
    S.bind (Selector.raw_signal editor.elements.arrangers) @@ fun arrangers ->
    S.bind (Input.Text.raw_signal editor.elements.remark) @@ fun remark ->
    S.bind (Input.Text.raw_signal editor.elements.disambiguation) @@ fun disambiguation ->
    S.bind (Input.Text.raw_signal editor.elements.content) @@ fun content ->
    S.const {tune; bars; key; structure; arrangers; remark; disambiguation; content}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Selector.signal_one editor.elements.tune) @@ fun tune ->
    RS.bind (Input.Text.signal editor.elements.bars) @@ fun bars ->
    RS.bind (Input.Text.signal editor.elements.key) @@ fun key ->
    RS.bind (Input.Text.signal editor.elements.structure) @@ fun structure ->
    RS.bind (Selector.signal_many editor.elements.arrangers) @@ fun arrangers ->
    RS.bind (Input.Text.signal editor.elements.remark) @@ fun remark ->
    RS.bind (Input.Text.signal editor.elements.disambiguation) @@ fun disambiguation ->
    RS.bind (Input.Text.signal editor.elements.content) @@ fun content ->
    RS.pure {tune; bars; key; structure; arrangers; remark; disambiguation; content}

  let with_or_without_local_storage ~text ~tune f =
    match text, tune with
    | (None, None) ->
      Lwt.return @@ Cutils.with_local_storage "VersionEditor" (module RawState) raw_state f
    | _ ->
      Lwt.return @@ f {RawState.empty with tune = (Option.value ~default: "" text, Option.value ~default: [] tune)}

  let create ~text ~tune : t Lwt.t =
    with_or_without_local_storage ~text ~tune @@ fun initial_state ->
    let (has_interacted, set_interacted) = S.create false in
    let set_interacted () = set_interacted true in
    let tune =
      Selector.make
        ~arity: Selector.one
        ~search: (fun slice input ->
            let%rlwt filter = Lwt.return (Model.Tune.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Tune.search slice filter
          )
        ~has_interacted
        ~serialise: Dancelor_common.Database.Entry.slug
        ~unserialise: Model.Tune.get
        initial_state.tune
    in
    let bars =
      Input.Text.make ~has_interacted initial_state.bars @@
      Option.to_result ~none: "The number of bars has to be an integer." % int_of_string_opt
    in
    let key =
      Input.Text.make ~has_interacted initial_state.key @@
      Option.to_result ~none: "Enter a valid key, eg. A of F#m." % Dancelor_common.Music.key_of_string_opt
    in
    let structure = Input.Text.make ~has_interacted initial_state.structure @@ Result.ok in
    let arrangers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search slice filter
          )
        ~serialise: Dancelor_common.Database.Entry.slug
        ~unserialise: Model.Person.get
        initial_state.arrangers
    in
    let remark = Input.Text.make ~has_interacted initial_state.remark @@ Result.ok in
    let disambiguation = Input.Text.make initial_state.disambiguation @@ Result.ok in
    let content =
      Input.Text.make ~has_interacted initial_state.content @@
      Result.of_string_nonempty ~empty: "Cannot be empty."
    in
    {
      elements = {tune; bars; key; structure; arrangers; remark; disambiguation; content};
      set_interacted;
    }

  let clear (editor : t) =
    Selector.clear editor.elements.tune;
    Input.Text.clear editor.elements.bars;
    Input.Text.clear editor.elements.key;
    Input.Text.clear editor.elements.structure;
    Selector.clear editor.elements.arrangers;
    Input.Text.clear editor.elements.remark;
    Input.Text.clear editor.elements.disambiguation;
    Input.Text.clear editor.elements.content

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {tune; bars; key; structure; arrangers; remark; disambiguation; content} ->
      Lwt.map Option.some @@
      Model.Version.save @@
      Model.Version.make
        ~tune
        ~bars
        ~key
        ~structure
        ~arrangers
        ~remark
        ~disambiguation
        ~content
        ()
end

let create ?on_save ?text ?tune () =
  let title = "Add a version" in
  Page.make ~title: (S.const title) @@
  L.div
    (
      let%lwt editor = Editor.create ~text ~tune in
      Lwt.return @@
      [
        h2 ~a: [a_class ["title"]] [txt title];
        form
          [
            Selector.render
              ~make_result: AnyResult.make_tune_result'
              ~field_name: ("Tune", "tune")
              ~model_name: "tune"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ TuneEditor.create ?on_save ~text ())
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
              ~field_name: ("Arrangers", "arranger")
              ~model_name: "person"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ PersonEditor.create ?on_save ~text ())
              editor.elements.arrangers;
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
            Button.group
              [
                Button.save
                  ~disabled: (S.map Option.is_none (Editor.state editor))
                  ~onclick: (fun () ->
                      editor.set_interacted ();
                      Fun.flip Lwt.map (Editor.submit editor) @@
                      Option.iter @@ fun version ->
                      Editor.clear editor;
                      match on_save with
                      | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_version (Dancelor_common.Database.Entry.slug version))
                      | Some on_save -> on_save version
                    )
                  ();
                Button.clear
                  ~onclick: (fun () -> Editor.clear editor)
                  ();
              ]
          ]
      ]
    )
