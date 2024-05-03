open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module SCDDB = Dancelor_common.SCDDB
module Model = Dancelor_client_model
module PageRouter = Dancelor_common.PageRouter
module Page = Dancelor_client_page

type ('name, 'scddb_id) gen = {
  name : 'name;
  scddb_id : 'scddb_id;
}
[@@deriving yojson]

module RawState = struct
  type t = (
    string,
    string
  ) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    scddb_id = "";
  }
end

module Editor = struct
  type t = (
    string Input.Text.t,
    SCDDB.entry_id option Input.Text.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.scddb_id) @@ fun scddb_id ->
    S.const {name; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.scddb_id) @@ fun scddb_id ->
    RS.pure {name; scddb_id}

  let create () : t =
    Utils.with_local_storage "PersonEditor" (module RawState) raw_state @@ fun initial_state ->
    let name = Input.Text.make initial_state.name @@
      Result.of_string_nonempty ~empty:"The name cannot be empty."
    in
    let scddb_id = Input.Text.make initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Person)
      % Option.of_string_nonempty
    in
    {name; scddb_id}

  let clear (editor : t) : unit =
    Input.Text.clear editor.name;
    Input.Text.clear editor.scddb_id

  let submit (editor : t) : Model.Person.t option Lwt.t =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; scddb_id} ->
      Lwt.map Option.some @@
      Model.Person.make_and_save
        ~name
        ?scddb_id
        ~modified_at: (Datetime.now ())
        ~created_at: (Datetime.now ())
        ()
end

let create ?on_save () =
  let title = "Add a person" in
  let editor = Editor.create () in
  Page.make ~title:(S.const title) @@
  div [
    h2 ~a:[a_class ["title"]] [txt title];
    form [
      Input.Text.render
        editor.name
        ~label: "Name"
        ~placeholder: "eg. John Doe";
      Input.Text.render
        editor.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/person/9999/";
      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun person ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_person (Model.Person.slug person))
              | Some on_save -> on_save person
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ];
    ]
  ]
