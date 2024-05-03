open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module Model = Dancelor_client_model
module SCDDB = Dancelor_common.SCDDB
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters
module Page = Dancelor_client_page

type ('tune, 'bars, 'key, 'structure, 'arrangers, 'remark, 'disambiguation, 'content) gen = {
  tune : 'tune;
  bars : 'bars;
  key : 'key;
  structure : 'structure;
  arrangers : 'arrangers;
  remark : 'remark;
  disambiguation : 'disambiguation;
  content : 'content;
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

  type t = (
    tune Slug.t option,
    string,
    string,
    string,
    person Slug.t list,
    string,
    string,
    string
  ) gen
  [@@deriving yojson]

  let empty = {
    tune = None;
    bars = "";
    key = "";
    structure = "";
    arrangers = [];
    remark = "";
    disambiguation = "";
    content = ""
  }

  let _key = "VersionEditor.RawState"
end

module Editor = struct
  type t = (
    Model.Tune.t Selector.t,
    int Input.Text.t,
    Model.Music.key Input.Text.t,
    string Input.Text.t,
    Model.Person.t ListSelector.t,
    string Input.Text.t,
    string Input.Text.t,
    string Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Selector.raw_signal editor.tune) @@ fun tune ->
    S.bind (Input.Text.raw_signal editor.bars) @@ fun bars ->
    S.bind (Input.Text.raw_signal editor.key) @@ fun key ->
    S.bind (Input.Text.raw_signal editor.structure) @@ fun structure ->
    S.bind (ListSelector.raw_signal editor.arrangers) @@ fun arrangers ->
    S.bind (Input.Text.raw_signal editor.remark) @@ fun remark ->
    S.bind (Input.Text.raw_signal editor.disambiguation) @@ fun disambiguation ->
    S.bind (Input.Text.raw_signal editor.content) @@ fun content ->
    S.const {tune; bars; key; structure; arrangers; remark; disambiguation; content}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Selector.signal_non_empty editor.tune) @@ fun tune ->
    RS.bind (Input.Text.signal editor.bars) @@ fun bars ->
    RS.bind (Input.Text.signal editor.key) @@ fun key ->
    RS.bind (Input.Text.signal editor.structure) @@ fun structure ->
    RS.bind (ListSelector.signal editor.arrangers) @@ fun arrangers ->
    RS.bind (Input.Text.signal editor.remark) @@ fun remark ->
    RS.bind (Input.Text.signal editor.disambiguation) @@ fun disambiguation ->
    RS.bind (Input.Text.signal editor.content) @@ fun content ->
    RS.pure {tune; bars; key; structure; arrangers; remark; disambiguation; content}

  let create () : t =
    Utils.with_local_storage (module RawState) raw_state @@ fun initial_state ->
    let tune = Selector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Tune.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Tune.search ~threshold ~slice filter
          )
        ~serialise: Model.Tune.slug
        ~unserialise: Model.Tune.get
        initial_state.tune
    in
    let bars = Input.Text.make initial_state.bars @@
      Option.to_result ~none:"Must be an integer" % int_of_string_opt
    in
    let key = Input.Text.make initial_state.key @@
      Option.to_result ~none:"Must be a valid key" % Model.Music.key_of_string_opt
    in
    let structure = Input.Text.make initial_state.structure @@ Result.ok in
    let arrangers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Model.Person.slug
        ~unserialise: Model.Person.get
        initial_state.arrangers
    in
    let remark = Input.Text.make initial_state.remark @@ Result.ok in
    let disambiguation = Input.Text.make initial_state.disambiguation @@ Result.ok in
    let content =
      Input.Text.make initial_state.content @@
      Result.of_string_nonempty ~empty: "Cannot be empty."
    in
    {tune; bars; key; structure; arrangers; remark; disambiguation; content}

  let clear (editor : t) =
    Selector.clear editor.tune;
    Input.Text.clear editor.bars;
    Input.Text.clear editor.key;
    Input.Text.clear editor.structure;
    ListSelector.clear editor.arrangers;
    Input.Text.clear editor.remark;
    Input.Text.clear editor.disambiguation;
    Input.Text.clear editor.content

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {tune; bars; key; structure; arrangers; remark; disambiguation; content} ->
      Lwt.map Option.some @@
      Model.Version.make_and_save
        ~tune
        ~bars
        ~key
        ~structure
        ~arrangers
        ~remark
        ~disambiguation
        ~content
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

let create ?on_save () =
  let title = "Add a version" in
  let editor = Editor.create () in
  Page.make ~title:(S.const title) @@
  div [
    h2 ~a:[a_class ["title"]] [txt title];

    form [
      Selector.render
        ~make_result: AnyResultNewAPI.make_tune_result'
        ~field_name: ("Tune", "tune")
        ~model_name: "tune"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ TuneEditor.create ?on_save ())
        editor.tune;
      Input.Text.render
        editor.bars
        ~label: "Number of bars"
        ~placeholder: "eg. 32 or 48";
      Input.Text.render
        editor.key
        ~label: "Key"
        ~placeholder: "eg. A or F#m";
      Input.Text.render
        editor.structure
        ~label: "Structure"
        ~placeholder: "eg. AABB or ABAB";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: ("Arrangers", "arranger")
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ PersonEditor.create ?on_save ())
        editor.arrangers;
      Input.Text.render
        editor.disambiguation
        ~label: "Disambiguation"
        ~placeholder: "If there are multiple versions with the same name, this field must be used to distinguish them.";
      Input.Text.render
        editor.remark
        ~label: "Remark"
        ~placeholder: "Any additional information that doesn't fit in the other fields.";
      Input.Text.render_as_textarea
        editor.content
        ~label: "LilyPond content"
        ~placeholder: "\\relative f' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    ...\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n    ...\n    }\n  }\n>>";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun version ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_version (Model.Version.slug version))
              | Some on_save -> on_save version
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ]
    ]
  ]
