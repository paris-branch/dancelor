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

type ('name, 'kind, 'devisers, 'date, 'disambiguation, 'two_chords, 'scddb_id) gen = {
  name : 'name;
  kind : 'kind;
  devisers : 'devisers;
  date : 'date;
  disambiguation : 'disambiguation;
  two_chords : 'two_chords;
  scddb_id : 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false

  type t = (
    string,
    string,
    person Slug.t list,
    string,
    string,
    (* bool, *)
    unit, (* FIXME *)
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    kind = "";
    devisers = [];
    date = "";
    disambiguation = "";
    two_chords = ();
    scddb_id = "";
  }

  let _key = "DanceEditor.RawState"
end

module Editor = struct
  type t = (
    string Input.Text.t,
    Model.Kind.Dance.t Input.Text.t,
    Model.Person.t ListSelector.t,
    PartialDate.t option Input.Text.t,
    string option Input.Text.t,
    (bool, string) Result.t Choices.t,
    SCDDB.entry_id option Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.kind) @@ fun kind ->
    S.bind (ListSelector.raw_signal editor.devisers) @@ fun devisers ->
    S.bind (Input.Text.raw_signal editor.date) @@ fun date ->
    S.bind (Input.Text.raw_signal editor.disambiguation) @@ fun disambiguation ->
    (* S.bind (Choices.raw_signal editor.two_chords) @@ fun two_chords -> *)
    let two_chords = () in (* FIXME *)
    S.bind (Input.Text.raw_signal editor.scddb_id) @@ fun scddb_id ->
    S.const {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.kind) @@ fun kind ->
    RS.bind (ListSelector.signal editor.devisers) @@ fun devisers ->
    RS.bind (Input.Text.signal editor.date) @@ fun date ->
    RS.bind (Input.Text.signal editor.disambiguation) @@ fun disambiguation ->
    RS.bind (Choices.signal editor.two_chords) @@ fun two_chords ->
    RS.bind (Input.Text.signal editor.scddb_id) @@ fun scddb_id ->
    RS.pure {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let create () : t =
    Utils.with_local_storage (module RawState) raw_state @@ fun initial_state ->
    let name = Input.Text.make initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind = Input.Text.make initial_state.kind @@
      Option.to_result ~none:"Not a valid kind" % Model.Kind.Dance.of_string_opt
    in
    let devisers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Model.Person.slug
        ~unserialise: Model.Person.get
        initial_state.devisers
    in
    let date = Input.Text.make initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string)
      % Option.of_string_nonempty
    in
    let disambiguation = Input.Text.make initial_state.disambiguation @@
      Result.ok % Option.of_string_nonempty
    in
    let two_chords = Choices.make_radios' [
        Choices.choice' ~value:false [txt "One chord"];
        Choices.choice' ~value:true [txt "Two chords"];
      ]
        ~name: "Number of chords"
        ~validate: (Option.to_result ~none:"A choice must be made")
    in
    let scddb_id = Input.Text.make initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Dance)
      % Option.of_string_nonempty
    in
    {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let clear editor =
    Input.Text.clear editor.name;
    Input.Text.clear editor.kind;
    ListSelector.clear editor.devisers;
    Input.Text.clear editor.date;
    Input.Text.clear editor.disambiguation;
    (* FIXME: clear two chords *)
    Input.Text.clear editor.scddb_id

  let submit editor =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; devisers; date; disambiguation; two_chords; scddb_id} ->
      Lwt.map Option.some @@
      Model.Dance.make_and_save
        ~name
        ~kind
        ~devisers
        ~two_chords
        ?scddb_id
        ?disambiguation
        ?date
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

let create ?on_save () =
  let title = "Add a dance" in
  let editor = Editor.create () in
  Page.make_new_api ~title:(S.const title) @@
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
        ~field_name: ("Devisers", "deviser")
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save () -> Page.get_content @@ PersonEditor.create ?on_save ())
        editor.devisers;
      Input.Text.render
        editor.date
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14";
      Choices.render
        editor.two_chords;
      Input.Text.render
        editor.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 14298 or https://my.strathspey.org/dd/dance/14298/";
      Input.Text.render
        editor.disambiguation
        ~label: "Disambiguation"
        ~placeholder: "If there are multiple dances with the same name, this field must be used to distinguish them.";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun dance ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_dance (Model.Dance.slug dance))
              | Some on_save -> on_save dance
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ]
    ]
  ]
