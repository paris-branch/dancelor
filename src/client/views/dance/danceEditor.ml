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
  name: 'name;
  kind: 'kind;
  devisers: 'devisers;
  date: 'date;
  disambiguation: 'disambiguation;
  two_chords: 'two_chords;
  scddb_id: 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type person = Model.Person.t
  let person_to_yojson _ = assert false
  let person_of_yojson _ = assert false

  type t =
    (string, string, (string * person Slug.t list), string, string,
     (* bool, *)
     unit, (* FIXME *)
     string) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    kind = "";
    devisers = ("", []);
    date = "";
    disambiguation = "";
    two_chords = ();
    scddb_id = "";
  }
end

module Editor = struct
  type t = {
    elements:
      (string Input.Text.t, Model.Kind.Dance.t Input.Text.t, (Selector.many, Model.Person.t) Selector.t, PartialDate.t option Input.Text.t, string option Input.Text.t, bool option Choices.t, SCDDB.entry_id option Input.Text.t) gen;
    set_interacted: unit -> unit;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.kind) @@ fun kind ->
    S.bind (Selector.raw_signal editor.elements.devisers) @@ fun devisers ->
    S.bind (Input.Text.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Input.Text.raw_signal editor.elements.disambiguation) @@ fun disambiguation ->
    (* S.bind (Choices.raw_signal editor.elements.two_chords) @@ fun two_chords -> *)
    let two_chords = () in (* FIXME *)
    S.bind (Input.Text.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.kind) @@ fun kind ->
    RS.bind (Selector.signal_many editor.elements.devisers) @@ fun devisers ->
    RS.bind (Input.Text.signal editor.elements.date) @@ fun date ->
    RS.bind (Input.Text.signal editor.elements.disambiguation) @@ fun disambiguation ->
    RS.bind (S.map Result.ok @@ Choices.signal editor.elements.two_chords) @@ fun two_chords ->
    RS.bind (Input.Text.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      Lwt.return @@ f {RawState.empty with name = text}
    | None ->
      Lwt.return @@
      Utils.with_local_storage "DanceEditor" (module RawState) raw_state f

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
    let devisers =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        ~serialise: Model.Person.slug
        ~unserialise: Model.Person.get
        initial_state.devisers
    in
    let date =
      Input.Text.make ~has_interacted initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string) %
      Option.of_string_nonempty
    in
    let disambiguation =
      Input.Text.make ~has_interacted initial_state.disambiguation @@
      Result.ok % Option.of_string_nonempty
    in
    let two_chords =
      Choices.make_radios
        [
          Choices.choice' [txt "I don't know"] ~checked: true;
          Choices.choice' ~value: false [txt "One chord"];
          Choices.choice' ~value: true [txt "Two chords"];
        ]
        ~name: "Number of chords"
    in
    let scddb_id =
      Input.Text.make ~has_interacted initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Dance) %
      Option.of_string_nonempty
    in
    {
      elements = {name; kind; devisers; date; disambiguation; two_chords; scddb_id};
      set_interacted;
    }

  let clear (editor : t) =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.kind;
    Selector.clear editor.elements.devisers;
    Input.Text.clear editor.elements.date;
    Input.Text.clear editor.elements.disambiguation;
    (* FIXME: clear two chords *)
    Input.Text.clear editor.elements.scddb_id

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; kind; devisers; date; disambiguation; two_chords; scddb_id} ->
      Lwt.map Option.some @@
      Model.Dance.make_and_save
        ~name
        ~kind
        ~devisers
        ?two_chords
        ?scddb_id
        ?disambiguation
        ?date
        ~modified_at: (Datetime.now ())
        (* FIXME: optional argument *)
        ~created_at: (Datetime.now ())
        (* FIXME: not even optional *)
        ()
end

let create ?on_save ?text () =
  let title = "Add a dance" in
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
              ~field_name: ("Devisers", "deviser")
              ~model_name: "person"
              ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ PersonEditor.create ?on_save ~text ())
              editor.elements.devisers;
            Input.Text.render
              editor.elements.date
              ~label: "Date of devising"
              ~placeholder: "eg. 2019 or 2012-03-14";
            Choices.render
              editor.elements.two_chords;
            Input.Text.render
              editor.elements.scddb_id
              ~label: "SCDDB ID"
              ~placeholder: "eg. 14298 or https://my.strathspey.org/dd/dance/14298/";
            Input.Text.render
              editor.elements.disambiguation
              ~label: "Disambiguation"
              ~placeholder: "If there are multiple dances with the same name, this field must be used to distinguish them.";
            Button.group
              [
                Button.save
                  ~disabled: (S.map Option.is_none (Editor.state editor))
                  ~onclick: (fun () ->
                      editor.set_interacted ();
                      Fun.flip Lwt.map (Editor.submit editor) @@
                      Option.iter @@ fun dance ->
                      Editor.clear editor;
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
    )
