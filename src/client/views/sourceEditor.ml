open Nes
open Common

open Components
open Html

type ('name, 'scddb_id, 'description) gen = {
  name: 'name;
  scddb_id: 'scddb_id;
  description: 'description;
}
[@@deriving yojson]

module RawState = struct
  type t =
  (string, string, string) gen
  [@@deriving yojson]

  let empty : t = {
    name = "";
    scddb_id = "";
    description = "";
  }
end

module Editor = struct
  type t = {
    elements:
    (string Input.t, SCDDB.entry_id option Input.t, string option Input.t) gen
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.bind (Input.raw_signal editor.elements.description) @@ fun description ->
    S.const {name; scddb_id; description}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.bind (Input.signal editor.elements.description) @@ fun description ->
    RS.pure {name; scddb_id; description}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      lwt @@ f {RawState.empty with name = text}
    | None ->
      lwt @@
        Cutils.with_local_storage "SourceEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.make
        ~type_: Text
        ~initial_value: initial_state.name
        ~label: "Name"
        ~placeholder: "eg. The Paris Book of Scottish Country Dances, volume 2"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    in
    let scddb_id =
      Input.make
        ~type_: Text
        ~initial_value: initial_state.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/publication/9999/"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Publication) %
            Option.of_string_nonempty
        )
        ()
    in
    let description =
      Input.make
        ~type_: Textarea
        ~initial_value: initial_state.name
        ~label: "Description"
        ~placeholder: "eg. Book provided by the RSCDS and containing almost all of the original tunes for the RSCDS dances. New editions come every now and then to add tunes for newly introduced RSCDS dances."
        ~validator: (function "" -> Ok None | s -> Ok (Some s))
        ()
    in
      {elements = {name; scddb_id; description}}

  let clear (editor : t) : unit =
    Input.clear editor.elements.name;
    Input.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Source.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> lwt_none
    | Some {name; scddb_id; description} ->
      some
      <$> Madge_client.call_exn Endpoints.Api.(route @@ Source Create) @@
          Model.Source.make ~name ?scddb_id ?description ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Page.make'
    ~title: (lwt "Add a source")
    ~on_load: (fun () -> Input.focus editor.elements.name)
    [Input.html editor.elements.name;
    Input.html editor.elements.scddb_id;
    Input.html editor.elements.description;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit editor) @@
          Option.iter @@ fun source ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Source created"
              [txt "The source ";
              Formatters.Source.name' ~link: true source;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to source"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_source @@ Entry.id source)
                  ();
              ]
          | Some on_save -> on_save source
        )
        ();
    ]
