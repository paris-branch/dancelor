open Nes
open Common

open Components
open Html
open Utils

type ('names, 'kind, 'devisers, 'date, 'disambiguation, 'two_chords, 'scddb_id) gen = {
  names: 'names;
  kind: 'kind;
  devisers: 'devisers;
  date: 'date;
  disambiguation: 'disambiguation;
  two_chords: 'two_chords;
  scddb_id: 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false

  type t = (
    string list,
    string,
    person Entry.Id.t option list,
    string,
    string,
    (* bool, *)
    unit,
    (* FIXME *)
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    names = [];
    kind = "";
    devisers = [];
    date = "";
    disambiguation = "";
    two_chords = ();
    scddb_id = "";
  }
end

module Editor = struct
  type t = {
    elements: (
      (string NonEmptyList.t, string list) Component.t,
      (Kind.Dance.t, string) Component.t,
      (Model.Person.t Entry.t list, Model.Person.t Entry.Id.t option list) Component.t,
      (PartialDate.t option, string) Component.t,
      (string option, string) Component.t,
      (bool option, string) Component.t,
      (SCDDB.entry_id option, string) Component.t
    ) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Component.raw_signal editor.elements.names) @@ fun names ->
    S.bind (Component.raw_signal editor.elements.kind) @@ fun kind ->
    S.bind (Component.raw_signal editor.elements.devisers) @@ fun devisers ->
    S.bind (Component.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Component.raw_signal editor.elements.disambiguation) @@ fun disambiguation ->
    (* S.bind (Choices.raw_signal editor.elements.two_chords) @@ fun two_chords -> *)
    let two_chords = () in
    (* FIXME *)
    S.bind (Component.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {names; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Component.signal editor.elements.names) @@ fun names ->
    RS.bind (Component.signal editor.elements.kind) @@ fun kind ->
    RS.bind (Component.signal editor.elements.devisers) @@ fun devisers ->
    RS.bind (Component.signal editor.elements.date) @@ fun date ->
    RS.bind (Component.signal editor.elements.disambiguation) @@ fun disambiguation ->
    RS.bind (Component.signal editor.elements.two_chords) @@ fun two_chords ->
    RS.bind (Component.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {names; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some "" -> lwt @@ f RawState.empty
    | Some text -> lwt @@ f {RawState.empty with names = [text]}
    | None -> lwt @@ Cutils.with_local_storage "DanceEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let names =
      ComponentList.make_non_empty
        (
          Input.prepare
            ~type_: Text
            ~label: "Name"
            ~placeholder: "eg. The Dusty Miller"
            ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
            ()
        )
        initial_state.names
    in
    let kind =
      Input.make
        ~type_: Text
        ~label: "Kind"
        ~placeholder: "eg. 8x32R or 2x(16R+16S)"
        ~validator: (Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." % Kind.Dance.of_string_opt)
        initial_state.kind
    in
    let devisers =
      ComponentList.make
        (
          Selector.prepare
            ~label: "Deviser"
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
        initial_state.devisers
    in
    let date =
      Input.make
        ~type_: Text
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % Option.to_result ~none: "Enter a valid date, eg. 2019, 2015-10, or 2012-03-14." % PartialDate.from_string) %
            Option.of_string_nonempty
        )
        initial_state.date
    in
    let disambiguation =
      Input.make
        ~type_: Text
        ~label: "Disambiguation"
        ~placeholder: "If there are multiple dances with the same name, this field must be used to distinguish them."
        ~validator: (ok % Option.of_string_nonempty)
        initial_state.disambiguation
    in
    let two_chords =
      Choices.make_radios
        ~label: "Number of chords"
        [
          Choices.choice' [txt "I don't know"] ~checked: true;
          Choices.choice' ~value: false [txt "One chord"];
          Choices.choice' ~value: true [txt "Two chords"];
        ]
    in
    let scddb_id =
      Input.make
        ~type_: Text
        ~label: "SCDDB ID"
        ~placeholder: "eg. 14298 or https://my.strathspey.org/dd/dance/14298/"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Dance) %
            Option.of_string_nonempty
        )
        initial_state.scddb_id
    in
      {elements = {names; kind; devisers; date; disambiguation; two_chords; scddb_id}}

  let clear (editor : t) =
    Component.clear editor.elements.names;
    Component.clear editor.elements.kind;
    Component.clear editor.elements.devisers;
    Component.clear editor.elements.date;
    Component.clear editor.elements.disambiguation;
    (* FIXME: clear two chords *)
    Component.clear editor.elements.scddb_id

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {names; kind; devisers; date; disambiguation; two_chords; scddb_id} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Create) @@
          Model.Dance.make
            ~names
            ~kind
            ~devisers
            ?two_chords
            ?scddb_id
            ?disambiguation
            ?date
            ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a dance")
    ~on_load: (fun () -> Component.focus editor.elements.names)
    [Component.html editor.elements.names;
    Component.html editor.elements.kind;
    Component.html editor.elements.devisers;
    Component.html editor.elements.date;
    Component.html editor.elements.two_chords;
    Component.html editor.elements.scddb_id;
    Component.html editor.elements.disambiguation;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun dance ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Dance created"
              [txt "The dance ";
              Formatters.Dance.name' ~link: true dance;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to dance"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_dance @@ Entry.id dance)
                  ();
              ]
          | Some on_save -> on_save dance
        )
        ();
    ]
