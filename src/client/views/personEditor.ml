open Nes
open Common

open Components
open Html

type ('name, 'scddb_id) gen = {
  name: 'name;
  scddb_id: 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  type t =
  (string, string) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    scddb_id = "";
  }
end

module Editor = struct
  type t = {
    elements:
    (string Input.t, SCDDB.entry_id option Input.t) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {name; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {name; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      lwt @@ f {RawState.empty with name = text}
    | None ->
      lwt @@
        Cutils.with_local_storage "PersonEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.make
        ~type_: Text
        ~initial_value: initial_state.name
        ~label: "Name"
        ~placeholder: "eg. John Doe"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    in
    let scddb_id =
      Input.make
        ~type_: Text
        ~initial_value: initial_state.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/person/9999/"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Person) %
            Option.of_string_nonempty
        )
        ()
    in
      {elements = {name; scddb_id}}

  let clear (editor : t) : unit =
    Input.clear editor.elements.name;
    Input.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Person.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {name; scddb_id} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Create) @@
          Model.Person.make ~name ?scddb_id ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a person")
    [Input.html editor.elements.name;
    Input.html editor.elements.scddb_id;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun person ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Person created"
              [txt "The person ";
              Formatters.Person.name' ~link: true person;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to person"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_person @@ Entry.id person)
                  ();
              ]
          | Some on_save -> on_save person
        )
        ();
    ]
