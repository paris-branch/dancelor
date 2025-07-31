open Nes
open Common

open Components
open Html
open Utils

type ('name, 'kind, 'conceptors, 'versions, 'order) gen = {
  name: 'name;
  kind: 'kind;
  conceptors: 'conceptors;
  versions: 'versions;
  order: 'order;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false
  type book = Model.Book.t
  let book_to_yojson _ = assert false
  let book_of_yojson _ = assert false
  type version = Model.Version.t
  let version_to_yojson _ = assert false
  let version_of_yojson _ = assert false

  type t =
  (string, string, person Entry.Id.t list, version Entry.Id.t list, string) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    kind = "";
    conceptors = [];
    versions = [];
    order = ""
  }
end

module Editor = struct
  type t = {
    elements:
    (string Input.Text.t, Kind.Dance.t Input.Text.t, (Selector.many, Model.Person.t) Selector.t, (Selector.many, Model.Version.t) Selector.t, Model.SetOrder.t Input.Text.t) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.kind) @@ fun kind ->
    S.bind (Selector.raw_signal editor.elements.conceptors) @@ fun conceptors ->
    S.bind (Selector.raw_signal editor.elements.versions) @@ fun versions ->
    S.bind (Input.Text.raw_signal editor.elements.order) @@ fun order ->
    S.const {name; kind; conceptors; versions; order}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.kind) @@ fun kind ->
    RS.bind (Selector.signal_many editor.elements.conceptors) @@ fun conceptors ->
    RS.bind (Selector.signal_many editor.elements.versions) @@ fun versions ->
    RS.bind (Input.Text.signal editor.elements.order) @@ fun order ->
    RS.pure {name; kind; conceptors; versions; order}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      lwt @@ f {RawState.empty with name = text}
    | None ->
      lwt @@
        Cutils.with_local_storage "SetEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.name
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    in
    let kind =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.kind
        ~label: "Kind"
        ~placeholder: "eg. 8x32R or 2x(16R+16S)"
        ~validator: (Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." % Kind.Dance.of_string_opt)
        ()
    in
    let conceptors =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Person.get
        initial_state.conceptors
    in
    let versions =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Version.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Version.get
        initial_state.versions
    in
    let order =
      Input.Text.make
        ~type_: Text
        ~initial_value: initial_state.order
        ~label: "Order"
        ~placeholder: "eg. 1,2,3,4,2,3,4,1"
        ~validator: (Option.to_result ~none: "Not a valid order." % Model.SetOrder.of_string_opt)
        ()
    in
    {
      elements = {name; kind; conceptors; versions; order};
    }

  let add_to_storage version =
    Cutils.update "SetEditor" (module RawState) @@ fun state ->
    {state with versions = state.versions @ [version]}

  let clear (editor : t) =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.kind;
    Selector.clear editor.elements.conceptors;
    Selector.clear editor.elements.versions;
    Input.Text.clear editor.elements.order

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {name; kind; conceptors; versions; order} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Create) @@
          Model.Set.make
            ~name
            ~kind
            ~conceptors
            ~contents: (List.map (fun version -> (version, Model.VersionParameters.none)) versions)
            ~order
            ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a set")
    [Input.Text.html editor.elements.name;
    Input.Text.html editor.elements.kind;
    Selector.render
      ~make_result: AnyResult.make_person_result'
      ~field_name: "Conceptors"
      ~model_name: "person"
      ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
      editor.elements.conceptors;
    Selector.render
      ~make_result: AnyResult.make_version_result'
      ~make_more_results: (fun version ->
        [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSvg.make version]]]
      )
      ~field_name: "Versions"
      ~model_name: "versions"
      ~create_dialog_content: (fun ?on_save text -> VersionEditor.create ?on_save ~text ())
      editor.elements.versions;
    Input.Text.html editor.elements.order;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun set ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Set created"
              [txt "The set ";
              Formatters.Set.name' ~link: true set;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to set"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_set @@ Entry.id set)
                  ();
              ]
          | Some on_save -> on_save set
        )
        ();
    ]
