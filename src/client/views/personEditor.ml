open Nes
open Common

open Js_of_ocaml
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
      (string Input.Text.t, SCDDB.entry_id option Input.Text.t) gen;
    set_interacted: unit -> unit;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.const {name; scddb_id}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.pure {name; scddb_id}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      Lwt.return @@ f {RawState.empty with name = text}
    | None ->
      Lwt.return @@
      Cutils.with_local_storage "PersonEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let (has_interacted, set_interacted) = S.create false in
    let set_interacted () = set_interacted true in
    let name =
      Input.Text.make ~has_interacted initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let scddb_id =
      Input.Text.make ~has_interacted initial_state.scddb_id @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Person) %
      Option.of_string_nonempty
    in
    {
      elements = {name; scddb_id};
      set_interacted;
    }

  let clear (editor : t) : unit =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Person.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; scddb_id} ->
      Lwt.map Option.some @@
      Model.Person.save @@
      Model.Person.make
        ~name
        ?scddb_id
        ()
end

let create ?on_save ?text () =
  let title = "Add a person" in
  Page.make
    ~title: (S.const title)
    [
      L.div
        (
          let%lwt editor = Editor.create ~text in
          Lwt.return @@
          [
            form
              [
                Input.Text.render
                  editor.elements.name
                  ~label: "Name"
                  ~placeholder: "eg. John Doe";
                Input.Text.render
                  editor.elements.scddb_id
                  ~label: "SCDDB ID"
                  ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/person/9999/";
                Button.group
                  [
                    Button.save
                      ~disabled: (S.map Option.is_none (Editor.state editor))
                      ~onclick: (fun () ->
                          editor.set_interacted ();
                          Fun.flip Lwt.map (Editor.submit editor) @@
                          Option.iter @@ fun person ->
                          Editor.clear editor;
                          match on_save with
                          | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_person (Entry.slug person))
                          | Some on_save -> on_save person
                        )
                      ();
                    Button.clear
                      ~onclick: (fun () -> Editor.clear editor)
                      ();
                  ];
              ]
          ]
        )
    ]
