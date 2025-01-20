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
module Database = Dancelor_common_database

type ('name, 'kind, 'composers, 'date, 'dances, 'remark, 'scddb_id) gen = {
  name: 'name;
  kind: 'kind;
  composers: 'composers;
  date: 'date;
  dances: 'dances;
  remark: 'remark;
  scddb_id: 'scddb_id;
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

  type t =
    (string, string, (string * person Slug.t list), string, (string * dance Slug.t list), string, string) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    kind = "";
    composers = ("", []);
    date = "";
    dances = ("", []);
    remark = "";
    scddb_id = "";
  }
end

module Editor = struct
  type t = {
    elements:
      (string Input.Text.t, Model.Kind.Base.t Input.Text.t, (Selector.many, Model.Person.t) Selector.t, PartialDate.t option Input.Text.t, (Selector.many, Model.Dance.t) Selector.t, string option Input.Text.t, SCDDB.entry_id option Input.Text.t) gen;
    set_interacted: unit -> unit;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.kind) @@ fun kind ->
    S.bind (Selector.raw_signal editor.elements.composers) @@ fun composers ->
    S.bind (Input.Text.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Selector.raw_signal editor.elements.dances) @@ fun dances ->
    S.bind (Input.Text.raw_signal editor.elements.remark) @@ fun remark ->
    S.bind (Input.Text.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {name; kind; composers; date; dances; remark; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.kind) @@ fun kind ->
    RS.bind (Selector.signal_many editor.elements.composers) @@ fun composers ->
    RS.bind (Input.Text.signal editor.elements.date) @@ fun date ->
    RS.bind (Selector.signal_many editor.elements.dances) @@ fun dances ->
    RS.bind (Input.Text.signal editor.elements.remark) @@ fun remark ->
    RS.bind (Input.Text.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {name; kind; composers; date; dances; remark; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      Lwt.return @@ f {RawState.empty with name = text}
    | None ->
      Lwt.return @@
      Utils.with_local_storage "TuneEditor" (module RawState) raw_state f

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
      Option.to_result ~none: "Enter a valid kind, eg. R or Strathspey." % Model.Kind.Base.of_string_opt
    in
    let composers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Database.Entry.slug
        ~unserialise: Model.Person.get
        initial_state.composers
    in
    let date =
      Input.Text.make ~has_interacted initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Enter a valid date, eg. 2019 or 2012-03-14" % PartialDate.from_string) %
      Option.of_string_nonempty
    in
    let dances =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Dance.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Dance.search ~threshold ~slice filter
          )
        ~serialise: Database.Entry.slug
        ~unserialise: Model.Dance.get
        initial_state.dances
    in
    let remark =
      Input.Text.make ~has_interacted initial_state.remark @@
      Result.ok % Option.of_string_nonempty
    in
    let scddb_id =
      Input.Text.make ~has_interacted initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Tune) %
      Option.of_string_nonempty
    in
    {
      elements = {name; kind; composers; date; dances; remark; scddb_id};
      set_interacted;
    }

  let clear (editor : t) : unit =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.kind;
    Selector.clear editor.elements.composers;
    Input.Text.clear editor.elements.date;
    Selector.clear editor.elements.dances;
    Input.Text.clear editor.elements.remark;
    Input.Text.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Tune.t Database.Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; composers; date; dances; remark; scddb_id} ->
      Lwt.map Option.some @@
      Model.Tune.save
        ~modified_at: (Datetime.now ())
        ~created_at: (Datetime.now ()) @@
      Model.Tune.make
        ~name
        ~kind
        ~composers
        ?date
        ~dances
        ?remark
        ?scddb_id
        ()
end

let create ?on_save ?text () =
  let title = "Add a tune" in
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
              ~placeholder: "eg. The Cairdin O't";
            Input.Text.render
              editor.elements.kind
              ~label: "Kind"
              ~placeholder: "eg. R or Strathspey";
            Selector.render
              ~make_result: AnyResult.make_person_result'
              ~field_name: ("Composers", "composer")
              ~model_name: "person"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ PersonEditor.create ?on_save ~text ())
              editor.elements.composers;
            Input.Text.render
              editor.elements.date
              ~label: "Date of devising"
              ~placeholder: "eg. 2019 or 2012-03-14";
            Selector.render
              ~make_result: AnyResult.make_dance_result'
              ~field_name: ("Dances", "dance")
              ~model_name: "dance"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ DanceEditor.create ?on_save ~text ())
              (* FIXME: Selector should just take a page *)
              editor.elements.dances;
            Input.Text.render
              editor.elements.remark
              ~label: "Remark"
              ~placeholder: "Any additional information that doesn't fit in the other fields.";
            Input.Text.render
              editor.elements.scddb_id
              ~label: "SCDDB ID"
              ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/";
            Button.group
              [
                Button.save
                  ~disabled: (S.map Option.is_none (Editor.state editor))
                  ~onclick: (fun () ->
                      editor.set_interacted ();
                      Fun.flip Lwt.map (Editor.submit editor) @@
                      Option.iter @@ fun tune ->
                      Editor.clear editor;
                      match on_save with
                      | None -> Dom_html.window##.location##.href := Js.string (PageRouter.href_tune (Database.Entry.slug tune))
                      | Some on_save -> on_save tune
                    )
                  ();
                Button.clear
                  ~onclick: (fun () -> Editor.clear editor)
                  ();
              ]
          ]
      ]
    )
