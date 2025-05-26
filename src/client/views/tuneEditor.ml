open Nes
open Common

open Js_of_ocaml
open Components
open Html
open Utils

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
  (string, string, person Slug.t list, string, dance Slug.t list, string, string) gen
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
end

module Editor = struct
  type t = {
    elements:
    (string Input.Text.t, Kind.Base.t Input.Text.t, (Selector.many, Model.Person.t) Selector.t, PartialDate.t option Input.Text.t, (Selector.many, Model.Dance.t) Selector.t, string option Input.Text.t, SCDDB.entry_id option Input.Text.t) gen;
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
        Cutils.with_local_storage "TuneEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.Text.make initial_state.name @@
        Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind =
      Input.Text.make initial_state.kind @@
        Option.to_result ~none: "Enter a valid kind, eg. R or Strathspey." % Kind.Base.of_string_opt
    in
    let composers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = Lwt.return (Filter.Person.from_string input) in
          Lwt.map Result.ok @@
            Madge_client.call_exn
              Endpoints.Api.(route @@ Person Search)
              slice
              filter
        )
        ~serialise: Entry.slug
        ~unserialise: Model.Person.get
        initial_state.composers
    in
    let date =
      Input.Text.make initial_state.date @@
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map Option.some % Option.to_result ~none: "Enter a valid date, eg. 2019 or 2012-03-14" % PartialDate.from_string) %
          Option.of_string_nonempty
    in
    let dances =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = Lwt.return (Filter.Dance.from_string input) in
          Lwt.map Result.ok @@
            Madge_client.call_exn
              Endpoints.Api.(route @@ Dance Search)
              slice
              filter
        )
        ~serialise: Entry.slug
        ~unserialise: Model.Dance.get
        initial_state.dances
    in
    let remark =
      Input.Text.make initial_state.remark @@
        Result.ok % Option.of_string_nonempty
    in
    let scddb_id =
      Input.Text.make initial_state.scddb_id @@
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Tune) %
          Option.of_string_nonempty
    in
    {
      elements = {name; kind; composers; date; dances; remark; scddb_id};
    }

  let clear (editor : t) : unit =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.kind;
    Selector.clear editor.elements.composers;
    Input.Text.clear editor.elements.date;
    Selector.clear editor.elements.dances;
    Input.Text.clear editor.elements.remark;
    Input.Text.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Tune.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; composers; date; dances; remark; scddb_id} ->
      Lwt.map Option.some @@
      Madge_client.call_exn Endpoints.Api.(route @@ Tune Create) @@
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
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (Lwt.return "Add a tune")
    [Input.Text.render
      editor.elements.name
      ~label: "Name"
      ~placeholder: "eg. The Cairdin O't";
    Input.Text.render
      editor.elements.kind
      ~label: "Kind"
      ~placeholder: "eg. R or Strathspey";
    Selector.render
      ~make_result: AnyResult.make_person_result'
      ~field_name: "Composers"
      ~model_name: "person"
      ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
      editor.elements.composers;
    Input.Text.render
      editor.elements.date
      ~label: "Date of devising"
      ~placeholder: "eg. 2019 or 2012-03-14";
    Selector.render
      ~make_result: AnyResult.make_dance_result'
      ~field_name: "Dances"
      ~model_name: "dance"
      ~create_dialog_content: (fun ?on_save text -> DanceEditor.create ?on_save ~text ())
      editor.elements.dances;
    Input.Text.render
      editor.elements.remark
      ~label: "Remark"
      ~placeholder: "Any additional information that doesn't fit in the other fields.";
    Input.Text.render
      editor.elements.scddb_id
      ~label: "SCDDB ID"
      ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/";
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          Fun.flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun tune ->
          Editor.clear editor;
          match on_save with
          | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_tune (Entry.slug tune))
          | Some on_save -> on_save tune
        )
        ();
    ]
