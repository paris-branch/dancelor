open Nes
open Common

open Components
open Html
open Utils

type ('name, 'date, 'contents) gen = {
  name: 'name;
  date: 'date;
  contents: 'contents;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type set = Model.Set.t
  let set_to_yojson _ = assert false
  let set_of_yojson _ = assert false
  type version = Model.Version.t
  let version_to_yojson _ = assert false
  let version_of_yojson _ = assert false

  type t = (
    string,
    string,
    (int option * [`Set of set Entry.Id.t option | `Version of version Entry.Id.t option] list) list
  ) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    date = "";
    contents = [];
  }
end

module State = struct
  type t = (
    string,
    PartialDate.t option,
    [`Set of Model.Set.t Entry.t | `Version of Model.Version.t Entry.t] list
  ) gen

  let to_raw_state (state : t) : RawState.t = {
    name = state.name;
    date = Option.fold ~none: "" ~some: PartialDate.to_string state.date;
    contents =
    List.map
      (function
        | `Set set -> (Some 0, [`Set (Some (Entry.id set)); `Version None])
        | `Version version -> (Some 1, [`Set None; `Version (Some (Entry.id version))])
      )
      state.contents;
  }

  exception Non_convertible

  let of_model (book : Model.Book.t Entry.t) : t Lwt.t =
    let%lwt contents = Model.Book.contents' book in
    let contents =
      List.map
        (function
          | Model.Book.Set (set, params) when params = Model.SetParameters.none -> `Set set
          | Model.Book.Version (version, params) when params = Model.VersionParameters.none -> `Version version
          | _ -> raise Non_convertible
        )
        contents
    in
    lwt
      {
        name = (Entry.value book).title;
        date = (Entry.value book).date;
        contents;
      }
end

module Editor = struct
  type t = {
    elements: (
      (string, string) Component.t,
      (PartialDate.t option, string) Component.t,
      ([`Set of Model.Set.t Entry.t | `Version of Model.Version.t Entry.t] list, (int option * [`Set of Model.Set.t Entry.Id.t option | `Version of Model.Version.t Entry.Id.t option] list) list) Component.t
    ) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Component.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Component.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Component.raw_signal editor.elements.contents) @@ fun contents ->
    S.const {name; date; contents}

  let state (editor : t) : State.t option S.t =
    S.map Result.to_option @@
    RS.bind (Component.signal editor.elements.name) @@ fun name ->
    RS.bind (Component.signal editor.elements.date) @@ fun date ->
    RS.bind (Component.signal editor.elements.contents) @@ fun contents ->
    RS.pure {name; date; contents}

  let with_or_without_local_storage ~text ~edit f =
    match (text, edit) with
    | Some text, _ ->
      lwt @@ f {RawState.empty with name = text}
    | _, Some id ->
      let%lwt book = Model.Book.get id in
      let%lwt raw_state = State.to_raw_state <$> State.of_model book in
      lwt @@ f raw_state
    | _, None ->
      lwt @@
        Cutils.with_local_storage "BookEditor" (module RawState) raw_state f

  let create ~text ~edit : t Lwt.t =
    with_or_without_local_storage ~text ~edit @@ fun initial_state ->
    let name =
      Input.make
        ~type_: Text
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller Book"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        initial_state.name
    in
    let date =
      Input.make
        ~type_: Text
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string) %
            Option.of_string_nonempty
        )
        initial_state.date
    in
    let contents =
      Star.make
        (
          Plus.prepare
            ~label: "Content"
            [
              Plus.wrap
                (fun x -> `Set x)
                (fun x -> `Set x)
                (function `Set x -> Some x | _ -> None)
                (
                  Selector.prepare
                    ~make_result: AnyResult.make_set_result'
                    ~make_more_results: (fun set -> [Utils.ResultRow.(make [cell ~a: [a_colspan 9999] [Formatters.Set.tunes' set]])])
                    ~label: "Set"
                    ~model_name: "set"
                    ~create_dialog_content: (fun ?on_save text -> SetEditor.create ?on_save ~text ())
                    ~search: (fun slice input ->
                      let%rlwt filter = lwt (Filter.Set.from_string input) in
                      ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Search) slice filter
                    )
                    ~serialise: Entry.id
                    ~unserialise: Model.Set.get
                    ()
                );
              Plus.wrap
                (fun x -> `Version x)
                (fun x -> `Version x)
                (function `Version x -> Some x | _ -> None)
                (
                  Selector.prepare
                    ~make_result: AnyResult.make_version_result'
                    ~make_more_results: (fun version -> [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSvg.make version]]])
                    ~label: "Version"
                    ~model_name: "version"
                    ~create_dialog_content: (fun ?on_save text -> VersionEditor.create ?on_save ~text ())
                    ~search: (fun slice input ->
                      let%rlwt filter = lwt (Filter.Version.from_string input) in
                      ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
                    )
                    ~serialise: Entry.id
                    ~unserialise: Model.Version.get
                    ()
                );
            ]
        )
        initial_state.contents
    in
    {
      elements = {name; date; contents};
    }

  let add_to_storage set =
    Cutils.update "BookEditor" (module RawState) @@ fun state ->
    {state with contents = state.contents @ [(Some 0, [`Set set; `Version None])]}

  let clear (editor : t) =
    Component.clear editor.elements.name;
    Component.clear editor.elements.date;
    Component.clear editor.elements.contents

  let submit ~edit (editor : t) =
    match (S.value (state editor), edit) with
    | (None, _) -> lwt_none
    | (Some {name; date; contents}, id) ->
      let contents =
        List.map
          (function
            | `Set set -> Model.Book.Set (set, Model.SetParameters.none)
            | `Version version -> Model.Book.Version (version, Model.VersionParameters.none)
          )
          contents
      in
      Lwt.map some @@
        (
          match id with
          | None -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create)
          | Some id -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) id
        )
          (Model.Book.make ~title: name ?date ~contents ())
end

let create ?on_save ?text ?edit () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text ~edit in
  Page.make'
    ~title: (lwt @@ (match edit with None -> "Add" | Some _ -> "Edit") ^ " a book")
    ~on_load: (fun () -> Component.focus editor.elements.name)
    [Component.html editor.elements.name;
    Component.html editor.elements.date;
    Component.html editor.elements.contents;
    ]
    ~buttons: [
      Button.clear
        ~onclick: (fun () -> Editor.clear editor)
        ();
      Button.save
        ~disabled: (S.map Option.is_none (Editor.state editor))
        ~onclick: (fun () ->
          flip Lwt.map (Editor.submit ~edit editor) @@
          Option.iter @@ fun book ->
          Editor.clear editor;
          match on_save with
          | None ->
            Components.Toast.open_
              ~title: "Book created"
              [txt "The book ";
              Formatters.Book.title_and_subtitle' book;
              txt " has been created successfully."]
              ~buttons: [
                Components.Button.make_a
                  ~label: "Go to book"
                  ~classes: ["btn-primary"]
                  ~href: (S.const @@ Endpoints.Page.href_book @@ Entry.id book)
                  ();
              ]
          | Some on_save -> on_save book
        )
        ();
    ]
