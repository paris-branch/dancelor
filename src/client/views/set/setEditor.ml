open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
open Dancelor_common
module Model = Dancelor_client_model
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters
module Page = Dancelor_client_page
module Database = Dancelor_common_database

type ('name, 'kind, 'conceptors, 'versions, 'order) gen = {
  name: 'name;
  kind: 'kind;
  conceptors: 'conceptors;
  versions: 'versions;
  order: 'order;
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

  type t =
    (string, string, (string * person Slug.t list), (string * version Slug.t list), string) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    kind = "";
    conceptors = ("", []);
    versions = ("", []);
    order = ""
  }
end

module Editor = struct
  type t = {
    elements:
      (string Input.Text.t, Model.Kind.Dance.t Input.Text.t, (Selector.many, Model.Person.t) Selector.t, (Selector.many, Model.Version.t) Selector.t, Model.SetOrder.t Input.Text.t) gen;
    set_interacted: unit -> unit;
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
      Lwt.return @@ f {RawState.empty with name = text}
    | None ->
      Lwt.return @@
      Utils.with_local_storage "SetEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let (has_interacted, set_interacted) = S.create false in
    let set_interacted () = set_interacted true in
    let name =
      Input.Text.make ~has_interacted initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind =
      Input.Text.make ~has_interacted initial_state.kind @@
      Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)" % Model.Kind.Dance.of_string_opt
    in
    let conceptors =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search slice filter
          )
        ~serialise: Database.Entry.slug
        ~unserialise: Model.Person.get
        initial_state.conceptors
    in
    let versions =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let%rlwt filter = Lwt.return (Model.Version.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Version.search slice filter
          )
        ~serialise: Database.Entry.slug
        ~unserialise: Model.Version.get
        initial_state.versions
    in
    let order =
      Input.Text.make ~has_interacted initial_state.order @@
      Option.to_result ~none: "Not a valid order." % Model.SetOrder.of_string_opt
    in
    {
      elements = {name; kind; conceptors; versions; order};
      set_interacted;
    }

  let add_to_storage version =
    Utils.update "SetEditor" (module RawState) @@ fun state ->
    {state with versions = (fst state.versions, snd state.versions @ [version])}

  let clear (editor : t) =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.kind;
    Selector.clear editor.elements.conceptors;
    Selector.clear editor.elements.versions;
    Input.Text.clear editor.elements.order

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; conceptors; versions; order} ->
      Lwt.map Option.some @@
      Model.Set.save @@
      Model.Set.make
        ~name
        ~kind
        ~conceptors
        ~contents: (List.map (fun version -> (version, Model.VersionParameters.none)) versions)
        ~order
        ()
end

let create ?on_save ?text () =
  let title = "Add a set" in
  Page.make ~title: (S.const title) @@
  L.div
    (
      let%lwt editor = Editor.create ~text in
      Lwt.return @@
      [
        h2 ~a: [a_class ["title"]] [txt title];
        form
          [
            Input.Text.render
              editor.elements.name
              ~label: "Name"
              ~placeholder: "eg. The Dusty Miller";
            Input.Text.render
              editor.elements.kind
              ~label: "Kind"
              ~placeholder: "eg. 8x32R or 2x(16R+16S)";
            Selector.render
              ~make_result: AnyResult.make_person_result'
              ~field_name: ("Conceptors", "conceptor")
              ~model_name: "person"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ PersonEditor.create ?on_save ~text ())
              editor.elements.conceptors;
            Selector.render
              ~make_result: AnyResult.make_version_result'
              ~make_more_results: (fun version ->
                  [
                    Dancelor_client_utils.ResultRow.make
                      ~classes: ["small-previsualisation"]
                      [
                        Dancelor_client_utils.ResultRow.cell
                          ~a: [a_colspan 9999]
                          [
                            object_
                              ~a: [
                                a_mime_type "image/svg+xml";
                                a_data (ApiRouter.(href @@ Version Svg) None (Database.Entry.slug version));
                              ]
                              [];
                          ]
                      ]
                  ]
                )
              ~field_name: ("Versions", "version")
              ~model_name: "versions"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ VersionEditor.create ?on_save ~text ())
              editor.elements.versions;
            Input.Text.render
              editor.elements.order
              ~label: "Order"
              ~placeholder: "eg. 1,2,3,4,2,3,4,1";
            Button.group
              [
                Button.save
                  ~disabled: (S.map Option.is_none (Editor.state editor))
                  ~onclick: (fun () ->
                      editor.set_interacted ();
                      Fun.flip Lwt.map (Editor.submit editor) @@
                      Option.iter @@ fun set ->
                      Editor.clear editor;
                      match on_save with
                      | None -> Dom_html.window##.location##.href := Js.string (PageRouter.href_set (Database.Entry.slug set))
                      | Some on_save -> on_save set
                    )
                  ();
                Button.clear
                  ~onclick: (fun () -> Editor.clear editor)
                  ();
              ]
          ]
      ]
    )
