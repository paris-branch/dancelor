open Nes
open Common

open Components
open Html
open Utils

type ('name, 'short_name, 'editors, 'scddb_id, 'description) gen = {
  name: 'name;
  short_name: 'short_name;
  editors: 'editors;
  scddb_id: 'scddb_id;
  description: 'description;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false

  type t = (
    string,
    string,
    person Entry.Id.t option list,
    string,
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    short_name = "";
    editors = [];
    scddb_id = "";
    description = "";
  }
end

module Editor = struct
  type t = {
    elements: (
      (string, string) Component.t,
      (string, string) Component.t,
      (Model.Person.t Entry.t list, Model.Person.t Entry.Id.t option list) Component.t,
      (SCDDB.entry_id option, string) Component.t,
      (string option, string) Component.t
    ) gen
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Component.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Component.raw_signal editor.elements.short_name) @@ fun short_name ->
    S.bind (Component.raw_signal editor.elements.editors) @@ fun editors ->
    S.bind (Component.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.bind (Component.raw_signal editor.elements.description) @@ fun description ->
    S.const {name; short_name; editors; scddb_id; description}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Component.signal editor.elements.name) @@ fun name ->
    RS.bind (Component.signal editor.elements.short_name) @@ fun short_name ->
    RS.bind (Component.signal editor.elements.editors) @@ fun editors ->
    RS.bind (Component.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.bind (Component.signal editor.elements.description) @@ fun description ->
    RS.pure {name; short_name; editors; scddb_id; description}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      lwt @@ f {RawState.empty with name = text}
    | None ->
      lwt @@
        Cutils.with_local_storage "SourceEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.make
        ~type_: Text
        ~label: "Name"
        ~placeholder: "eg. The Paris Book of Scottish Country Dances, volume 2"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        initial_state.name
    in
    let short_name =
      Input.make
        ~type_: Text
        ~label: "Short name"
        ~placeholder: "eg. Paris Book 2"
        ~validator: ok
        initial_state.short_name
    in
    let editors =
      Star.make
        (
          Selector.prepare
            ~label: "Editor"
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Person.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
            )
            ~serialise: Entry.id
            ~unserialise: Model.Person.get
            ~make_result: AnyResult.make_person_result'
            ~model_name: "person"
            ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
            ()
        )
        initial_state.editors
    in
    let scddb_id =
      Input.make
        ~type_: Text
        ~label: "SCDDB ID"
        ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/publication/9999/"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Publication) %
            Option.of_string_nonempty
        )
        initial_state.scddb_id
    in
    let description =
      Input.make
        ~type_: Textarea
        ~label: "Description"
        ~placeholder: "eg. Book provided by the RSCDS and containing almost all of the original tunes for the RSCDS dances. New editions come every now and then to add tunes for newly introduced RSCDS dances."
        ~validator: (function "" -> Ok None | s -> Ok (Some s))
        initial_state.name
    in
      {elements = {name; short_name; editors; scddb_id; description}}

  let clear (editor : t) : unit =
    Component.clear editor.elements.name;
    Component.clear editor.elements.short_name;
    Component.clear editor.elements.editors;
    Component.clear editor.elements.scddb_id;
    Component.clear editor.elements.description

  let submit (editor : t) : Model.Source.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {name; short_name; editors; scddb_id; description} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Source Create) @@
          Model.Source.make ~name ~short_name ~editors ?scddb_id ?description ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a source")
    ~on_load: (fun () -> Component.focus editor.elements.name)
    [Component.html editor.elements.name;
    Component.html editor.elements.short_name;
    Component.html editor.elements.editors;
    Component.html editor.elements.scddb_id;
    Component.html editor.elements.description;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun source ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Source created"
              [txt "The source ";
              Formatters.Source.name' ~link: true source;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to source"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_source @@ Entry.id source)
                  ();
              ]
          | Some on_save -> on_save source
        )
        ();
    ]
