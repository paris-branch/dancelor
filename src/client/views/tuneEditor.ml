open Nes
open Common

open Components
open Html
open Utils

type ('names, 'kind, 'composers, 'date, 'dances, 'remark, 'scddb_id) gen = {
  names: 'names;
  kind: 'kind;
  composers: 'composers;
  date: 'date;
  dances: 'dances;
  remark: 'remark;
  scddb_id: 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false
  type dance = Model.Dance.t
  let dance_to_yojson _ = assert false
  let dance_of_yojson _ = assert false

  type t = (
    string list,
    string,
    person Entry.Id.t option list,
    string,
    dance Entry.Id.t option list,
    string,
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    names = [];
    kind = "";
    composers = [];
    date = "";
    dances = [];
    remark = "";
    scddb_id = "";
  }
end

module Editor = struct
  type t = {
    elements: (
      (string NonEmptyList.t, string list) Component.t,
      (Kind.Base.t, string) Component.t,
      (Model.Person.t Entry.t list, Model.Person.t Entry.Id.t option list) Component.t,
      (PartialDate.t option, string) Component.t,
      (Model.Dance.t Entry.t list, Model.Dance.t Entry.Id.t option list) Component.t,
      (string option, string) Component.t,
      (SCDDB.entry_id option, string) Component.t
    ) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Component.raw_signal editor.elements.names) @@ fun names ->
    S.bind (Component.raw_signal editor.elements.kind) @@ fun kind ->
    S.bind (Component.raw_signal editor.elements.composers) @@ fun composers ->
    S.bind (Component.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Component.raw_signal editor.elements.dances) @@ fun dances ->
    S.bind (Component.raw_signal editor.elements.remark) @@ fun remark ->
    S.bind (Component.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {names; kind; composers; date; dances; remark; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Component.signal editor.elements.names) @@ fun names ->
    RS.bind (Component.signal editor.elements.kind) @@ fun kind ->
    RS.bind (Component.signal editor.elements.composers) @@ fun composers ->
    RS.bind (Component.signal editor.elements.date) @@ fun date ->
    RS.bind (Component.signal editor.elements.dances) @@ fun dances ->
    RS.bind (Component.signal editor.elements.remark) @@ fun remark ->
    RS.bind (Component.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {names; kind; composers; date; dances; remark; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some "" -> lwt @@ f RawState.empty
    | Some text -> lwt @@ f {RawState.empty with names = [text]}
    | None -> lwt @@ Cutils.with_local_storage "TuneEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let names =
      ComponentList.make_non_empty
        (
          Input.prepare
            ~label: "Name"
            ~type_: Text
            ~placeholder: "eg. The Cairdin O't"
            ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
            ()
        )
        initial_state.names
    in
    let kind =
      Input.make
        ~type_: Text
        ~label: "Kind"
        ~placeholder: "eg. R or Strathspey"
        ~validator: (Option.to_result ~none: "Enter a valid kind, eg. R or Strathspey." % Kind.Base.of_string_opt)
        initial_state.kind
    in
    let composers =
      ComponentList.make
        (
          Selector.prepare
            ~make_result: AnyResult.make_person_result'
            ~label: "Composer"
            ~model_name: "person"
            ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Person.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
            )
            ~serialise: Entry.id
            ~unserialise: Model.Person.get
            ()
        )
        initial_state.composers
    in
    let date =
      Input.make
        ~type_: Text
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % Option.to_result ~none: "Enter a valid date, eg. 2019 or 2012-03-14" % PartialDate.from_string) %
            Option.of_string_nonempty
        )
        initial_state.date
    in
    let dances =
      ComponentList.make
        (
          Selector.prepare
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Dance.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
            )
            ~serialise: Entry.id
            ~unserialise: Model.Dance.get
            ~make_result: AnyResult.make_dance_result'
            ~label: "Dance"
            ~model_name: "dance"
            ~create_dialog_content: (fun ?on_save text -> DanceEditor.create ?on_save ~text ())
            ()
        )
        initial_state.dances
    in
    let remark =
      Input.make
        ~type_: Text
        ~label: "Remark"
        ~placeholder: "Any additional information that doesn't fit in the other fields."
        ~validator: (ok % Option.of_string_nonempty)
        initial_state.remark
    in
    let scddb_id =
      Input.make
        ~type_: Text
        ~label: "SCDDB ID"
        ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Tune) %
            Option.of_string_nonempty
        )
        initial_state.scddb_id
    in
    {
      elements = {names; kind; composers; date; dances; remark; scddb_id};
    }

  let clear (editor : t) : unit =
    Component.clear editor.elements.names;
    Component.clear editor.elements.kind;
    Component.clear editor.elements.composers;
    Component.clear editor.elements.date;
    Component.clear editor.elements.dances;
    Component.clear editor.elements.remark;
    Component.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Tune.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {names; kind; composers; date; dances; remark; scddb_id} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Tune Create) @@
          Model.Tune.make ~names ~kind ~composers ?date ~dances ?remark ?scddb_id ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a tune")
    ~on_load: (fun () -> Component.focus editor.elements.names)
    [Component.html editor.elements.names;
    Component.html editor.elements.kind;
    Component.html editor.elements.composers;
    Component.html editor.elements.date;
    Component.html editor.elements.dances;
    Component.html editor.elements.remark;
    Component.html editor.elements.scddb_id;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun tune ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Tune created"
              [txt "The tune ";
              Formatters.Tune.name' ~link: true tune;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to tune"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_tune @@ Entry.id tune)
                  ();
              ]
          | Some on_save -> on_save tune
        )
        ();
    ]
