open Nes
open Common

open Js_of_ocaml
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
    (string Input.Text.t, SCDDB.entry_id option Input.Text.t, string option Input.Text.t) gen
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.scddb_id) @@ fun scddb_id ->
    S.bind (Input.Text.raw_signal editor.elements.description) @@ fun description ->
    S.const {name; scddb_id; description}

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.scddb_id) @@ fun scddb_id ->
    RS.bind (Input.Text.signal editor.elements.description) @@ fun description ->
    RS.pure {name; scddb_id; description}

  let with_or_without_local_storage ~text f =
    match text with
    | Some text ->
      Lwt.return @@ f {RawState.empty with name = text}
    | None ->
      Lwt.return @@
        Cutils.with_local_storage "SourceEditor" (module RawState) raw_state f

  let create ~text : t Lwt.t =
    with_or_without_local_storage ~text @@ fun initial_state ->
    let name =
      Input.Text.make initial_state.name @@
        Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let scddb_id =
      Input.Text.make initial_state.scddb_id @@
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Publication) %
          Option.of_string_nonempty
    in
    let description =
      Input.Text.make initial_state.name @@
        (function "" -> Ok None | s -> Ok (Some s))
    in
      {elements = {name; scddb_id; description}}

  let clear (editor : t) : unit =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.scddb_id

  let submit (editor : t) : Model.Source.t Entry.t option Lwt.t =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; scddb_id; description} ->
      Lwt.map Option.some @@
      Madge_client.call_exn Endpoints.Api.(route @@ Source Create) @@
      Model.Source.make
        ~name
        ?scddb_id
        ?description
        ()
end

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text in
  Lwt.return @@
    Page.make
      ~title: (Lwt.return "Add a source")
      [Input.Text.render
        editor.elements.name
        ~label: "Name"
        ~placeholder: "eg. The Paris Book of Scottish Country Dances, volume 2";
      Input.Text.render
        editor.elements.scddb_id
        ~label: "SCDDB ID"
        ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/publication/9999/";
      Input.Text.render_as_textarea
        editor.elements.description
        ~label: "Description"
        ~placeholder: "eg. Book provided by the RSCDS and containing almost all of the original tunes for the RSCDS dances. New editions come every now and then to add tunes for newly introduced RSCDS dances.";
      ]
      ~buttons: [
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
            Fun.flip Lwt.map (Editor.submit editor) @@
            Option.iter @@ fun source ->
            Editor.clear editor;
            match on_save with
            | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_source (Entry.slug source))
            | Some on_save -> on_save source
          )
          ();
      ]
