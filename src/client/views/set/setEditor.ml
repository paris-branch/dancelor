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

type ('name, 'kind, 'conceptors, 'versions, 'order) gen = {
  name : 'name;
  kind : 'kind;
  conceptors : 'conceptors;
  versions : 'versions;
  order : 'order;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false
  type book = Model.Book.t
  let book_to_yojson _ = assert false
  let book_of_yojson _ = assert false
  type version = Model.Version.t
  let version_to_yojson _ = assert false
  let version_of_yojson _ = assert false

  type t = (
    string,
    string,
    person Slug.t list,
    version Slug.t list,
    string
  ) gen
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
  type t = (
    string Input.Text.t,
    Model.Kind.Dance.t Input.Text.t,
    Model.Person.t ListSelector.t,
    Model.Version.t ListSelector.t,
    Model.SetOrder.t Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.kind) @@ fun kind ->
    S.bind (ListSelector.raw_signal editor.conceptors) @@ fun conceptors ->
    S.bind (ListSelector.raw_signal editor.versions) @@ fun versions ->
    S.bind (Input.Text.raw_signal editor.order) @@ fun order ->
    S.const {name; kind; conceptors; versions; order}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.kind) @@ fun kind ->
    RS.bind (ListSelector.signal editor.conceptors) @@ fun conceptors ->
    RS.bind (ListSelector.signal editor.versions) @@ fun versions ->
    RS.bind (Input.Text.signal editor.order) @@ fun order ->
    RS.pure {name; kind; conceptors; versions; order}

  let create () : t =
    Utils.with_local_storage "SetEditor" (module RawState) raw_state @@ fun initial_state ->
    let name = Input.Text.make initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind = Input.Text.make initial_state.kind @@
      Option.to_result ~none: "Not a valid kind." % Model.Kind.Dance.of_string_opt
    in
    let conceptors = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Model.Person.slug
        ~unserialise: Model.Person.get
        initial_state.conceptors
    in
    let versions = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Version.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Version.search ~threshold ~slice filter
          )
        ~serialise: Model.Version.slug
        ~unserialise: Model.Version.get
        initial_state.versions
    in
    let order = Input.Text.make initial_state.order @@
      Option.to_result ~none:"Not a valid order." % Model.SetOrder.of_string_opt
    in
    {name; kind; conceptors; versions; order}

  let add_to_storage version =
    Utils.update "SetEditor" (module RawState) @@ fun state ->
    { state with versions = state.versions @ [version] }

  let clear (editor : t) =
    Input.Text.clear editor.name;
    Input.Text.clear editor.kind;
    ListSelector.clear editor.conceptors;
    ListSelector.clear editor.versions;
    Input.Text.clear editor.order

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; conceptors; versions; order} ->
      Lwt.map Option.some @@
      Model.Set.make_and_save
        ~name
        ~kind
        ~conceptors
        ~contents: (List.map (fun version -> (version, Model.VersionParameters.none)) versions)
        ~order
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

let create ?on_save () =
  let title = "Add a set" in
  let editor = Editor.create () in
  Page.make ~title:(S.const title) @@
  div [
    h2 ~a:[a_class ["title"]] [txt title];

    form [
      Input.Text.render
        editor.name
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller";
      Input.Text.render
        editor.kind
        ~label: "Kind"
        ~placeholder: "eg. 8x32R or 2x(16R+16S))";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: ("Conceptors", "conceptor")
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ PersonEditor.create ?on_save ())
        editor.conceptors;
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_version_result'
        ~field_name: ("Versions", "version")
        ~model_name: "versions"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ VersionEditor.create ?on_save ())
        editor.versions;
      Input.Text.render
        editor.order
        ~label: "Order"
        ~placeholder: "eg. 1,2,3,4,2,3,4,1";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun set ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_set (Model.Set.slug set))
              | Some on_save -> on_save set
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ]
    ]
  ]
