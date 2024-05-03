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

type ('name, 'kind, 'composers, 'date, 'dances, 'remark, 'scddb_id) gen = {
  name : 'name;
  kind : 'kind;
  composers : 'composers;
  date : 'date;
  dances : 'dances;
  remark : 'remark;
  scddb_id : 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false
  type dance = Model.Dance.t
  let dance_to_yojson _ = assert false
  let dance_of_yojson _ = assert false

  type t = (
    string,
    string,
    person Slug.t list,
    string,
    dance Slug.t list,
    string,
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    kind = "";
    composers = [];
    date = "";
    dances = [];
    remark = "";
    scddb_id = "";
  }

  let _key = "TuneEditor.RawState"
end

module Editor = struct
  type t = (
    string Input.Text.t,
    Model.Kind.Base.t Input.Text.t,
    Model.Person.t ListSelector.t,
    PartialDate.t option Input.Text.t,
    Model.Dance.t ListSelector.t,
    string option Input.Text.t,
    SCDDB.entry_id option Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.kind) @@ fun kind ->
    S.bind (ListSelector.raw_signal editor.composers) @@ fun composers ->
    S.bind (Input.Text.raw_signal editor.date) @@ fun date ->
    S.bind (ListSelector.raw_signal editor.dances) @@ fun dances ->
    S.bind (Input.Text.raw_signal editor.remark) @@ fun remark ->
    S.bind (Input.Text.raw_signal editor.scddb_id) @@ fun scddb_id ->
    S.const {name; kind; composers; date; dances; remark; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.kind) @@ fun kind ->
    RS.bind (ListSelector.signal editor.composers) @@ fun composers ->
    RS.bind (Input.Text.signal editor.date) @@ fun date ->
    RS.bind (ListSelector.signal editor.dances) @@ fun dances ->
    RS.bind (Input.Text.signal editor.remark) @@ fun remark ->
    RS.bind (Input.Text.signal editor.scddb_id) @@ fun scddb_id ->
    RS.pure {name; kind; composers; date; dances; remark; scddb_id}

  let create () : t =
    Utils.with_local_storage (module RawState) raw_state @@ fun initial_state ->
    let name = Input.Text.make initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind = Input.Text.make initial_state.kind @@
      Option.to_result ~none:"Not a valid kind" % Model.Kind.Base.of_string_opt
    in
    let composers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Model.Person.slug
        ~unserialise: Model.Person.get
        initial_state.composers
    in
    let date = Input.Text.make initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string)
      % Option.of_string_nonempty
    in
    let dances = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Dance.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Dance.search ~threshold ~slice filter
          )
        ~serialise: Model.Dance.slug
        ~unserialise: Model.Dance.get
        initial_state.dances
    in
    let remark = Input.Text.make initial_state.remark @@
      Result.ok % Option.of_string_nonempty
    in
    let scddb_id = Input.Text.make initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Tune)
      % Option.of_string_nonempty
    in
    {name; kind; composers; date; dances; remark; scddb_id}

  let clear editor =
    Input.Text.clear editor.name;
    Input.Text.clear editor.kind;
    ListSelector.clear editor.composers;
    Input.Text.clear editor.date;
    ListSelector.clear editor.dances;
    Input.Text.clear editor.remark;
    Input.Text.clear editor.scddb_id

  let submit editor =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; composers; date; dances; remark; scddb_id} ->
      Lwt.map Option.some @@
      Model.Tune.make_and_save
        ~name
        ~kind
        ~composers
        ?date
        ~dances
        ?remark
        ?scddb_id
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

let create ?on_save () =
  let title = "Add a tune" in
  let editor = Editor.create () in
  Page.make_new_api ~title:(S.const title) @@
  div [
    h2 ~a:[a_class ["title"]] [txt title];

    form [
      Input.Text.render
        editor.name
        ~label: "Name"
        ~placeholder: "eg. The Cairdin O't";
      Input.Text.render
        editor.kind
        ~label: "Kind"
        ~placeholder:"eg. R or Strathspey";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: ("Composers", "composer")
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ PersonEditor.create ?on_save ())
        editor.composers;
      Input.Text.render
        editor.date
        ~label: "Date of devising"
        ~placeholder:"eg. 2019 or 2012-03-14";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_dance_result'
        ~field_name: ("Dances", "dance")
        ~model_name: "dance"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ DanceEditor.create ?on_save ()) (* FIXME: ListSelector should just take a page *)
        editor.dances;
      Input.Text.render
        editor.remark
        ~label: "Remark"
        ~placeholder: "Any additional information that doesn't fit in the other fields.";
      Input.Text.render
        editor.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun tune ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_tune (Model.Tune.slug tune))
              | Some on_save -> on_save tune
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ]
    ]
  ]
