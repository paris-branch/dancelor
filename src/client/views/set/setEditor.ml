open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module Model = Dancelor_client_model
module SCDDB = Dancelor_common.SCDDB
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters

type ('name, 'kind, 'conceptors, 'for_book, 'versions, 'order) gen = {
  name : 'name;
  kind : 'kind;
  conceptors : 'conceptors;
  for_book : 'for_book;
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
    book Slug.t option,
    version Slug.t list,
    string
  ) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    kind = "";
    conceptors = [];
    for_book = None;
    versions = [];
    order = ""
  }

  let _key = "SetEditor.RawState"
end

module Editor = struct
  type t = (
    string Input.Text.t,
    Model.Kind.Dance.t Input.Text.t,
    Model.Person.t ListSelector.t,
    Model.Book.t Selector.t,
    Model.Version.t ListSelector.t,
    Model.SetOrder.t Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.kind) @@ fun kind ->
    S.bind (ListSelector.raw_signal editor.conceptors) @@ fun conceptors ->
    S.bind (Selector.raw_signal editor.for_book) @@ fun for_book ->
    S.bind (ListSelector.raw_signal editor.versions) @@ fun versions ->
    S.bind (Input.Text.raw_signal editor.order) @@ fun order ->
    S.const {name; kind; conceptors; for_book; versions; order}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.kind) @@ fun kind ->
    RS.bind (ListSelector.signal editor.conceptors) @@ fun conceptors ->
    RS.bind (Selector.signal editor.for_book) @@ fun for_book ->
    RS.bind (ListSelector.signal editor.versions) @@ fun versions ->
    RS.bind (Input.Text.signal editor.order) @@ fun order ->
    RS.pure {name; kind; conceptors; for_book; versions; order}

  let create () : t =
    Utils.with_local_storage (module RawState) raw_state @@ fun initial_state ->
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
    let for_book = Selector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Book.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Book.search ~threshold ~slice filter
          )
        ~serialise: Model.Book.slug
        ~unserialise: Model.Book.get
        initial_state.for_book
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
    {name; kind; conceptors; for_book; versions; order}

  let add_to_storage version =
    Utils.update (module RawState) @@ fun state ->
    { state with versions = state.versions @ [version] }

  let clear (editor : t) =
    Input.Text.clear editor.name;
    Input.Text.clear editor.kind;
    ListSelector.clear editor.conceptors;
    Selector.clear editor.for_book;
    ListSelector.clear editor.versions;
    Input.Text.clear editor.order

  (* FIXME: get rid of this shitty [for_book] business. This involves being able
     to edit books. *)
  let submit_updated_book set = function
    | None -> Lwt.return_unit
    | Some book ->
      let slug = Model.Book.slug book in
      let title = Model.Book.title book in
      let date = Model.Book.date book in
      let%lwt contents = Model.Book.contents book in
      let contents = contents @ [Set (set, Model.SetParameters.none)] in
      let modified_at = Datetime.now () in
      let created_at = Model.Book.created_at book in
      Model.Book.update ~slug ~title ?date ~contents ~modified_at ~created_at ()

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; conceptors; for_book; versions; order} ->
      let%lwt set = Model.Set.make_and_save
          ~name
          ~kind
          ~conceptors
          ~contents: (List.map (fun version -> (version, Model.VersionParameters.none)) versions)
          ~order
          ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
          ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
          ()
      in
      submit_updated_book set for_book;%lwt
      Lwt.return_some set
end

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let refresh _ = ()
let contents t = t.content
let init t = refresh t

let createNewAPI ?on_save () =
  let editor = Editor.create () in
  div [
    h2 ~a:[a_class ["title"]] [txt "Add a version"];

    form [
      Input.Text.render editor.name ~placeholder:"Name";
      Input.Text.render editor.kind ~placeholder:"Kind";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: "conceptor"
        ~model_name: "person"
        ~create_dialog_content: PersonEditor.createNewAPI
        editor.conceptors;
      (* Selector.render *)
      (*   ~make_result: AnyResultNewAPI.make_book_result' *)
      (*   ~field_name: "for book" *)
      (*   ~model_name: "book" *)
      (*   ~create_dialog_content: BookEditor.createNewAPI *)
      (*   editor.book; *)
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_version_result'
        ~field_name: "version"
        ~model_name: "versions"
        ~create_dialog_content: VersionEditor.createNewAPI
        editor.versions;
      Input.Text.render editor.order ~placeholder:"Order";

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

let create ?on_save page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  Lwt.async (fun () ->
      document##.title := Js.string "Add a set | Dancelor";
      Lwt.return ()
    );
  Dom.appendChild content (To_dom.of_div (createNewAPI ?on_save ()));
  {page; content}
