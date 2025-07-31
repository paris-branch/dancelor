open Nes
open Common

open Components
open Html
open Utils

type ('name, 'date, 'sets) gen = {
  name: 'name;
  date: 'date;
  sets: 'sets;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise ids. *)
  type set = Model.Set.t
  let set_to_yojson _ = assert false
  let set_of_yojson _ = assert false

  type t =
  (string, string, set Entry.Id.t list) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    date = "";
    sets = [];
  }
end

module State = struct
  type t =
  (string, PartialDate.t option, Model.Set.t Entry.t list) gen

  let to_raw_state (state : t) : RawState.t = {
    name = state.name;
    date = Option.fold ~none: "" ~some: PartialDate.to_string state.date;
    sets = List.map Entry.id state.sets;
  }

  exception Non_convertible

  let of_model (book : Model.Book.t Entry.t) : t Lwt.t =
    let%lwt contents = Model.Book.contents' book in
    let sets =
      List.map
        (function
          | Model.Book.Set (set, params) when params = Model.SetParameters.none -> set
          | _ -> raise Non_convertible
        )
        contents
    in
    lwt
      {
        name = (Entry.value book).title;
        date = (Entry.value book).date;
        sets;
      }
end

module Editor = struct
  type t = {
    elements:
    (string Input.t, PartialDate.t option Input.t, (Selector.many, Model.Set.t) Selector.t) gen;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Selector.raw_signal editor.elements.sets) @@ fun sets ->
    S.const {name; date; sets}

  let state (editor : t) : State.t option S.t =
    S.map Result.to_option @@
    RS.bind (Input.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.signal editor.elements.date) @@ fun date ->
    RS.bind (Selector.signal_many editor.elements.sets) @@ fun sets ->
    RS.pure {name; date; sets}

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
        ~initial_value: initial_state.name
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller Book"
        ~validator: (Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    in
    let date =
      Input.make
        ~type_: Text
        ~initial_value: initial_state.date
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14"
        ~validator: (
          Option.fold
            ~none: (Ok None)
            ~some: (Result.map some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string) %
            Option.of_string_nonempty
        )
        ()
    in
    let sets =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Set.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Set.get
        initial_state.sets
    in
    {
      elements = {name; date; sets};
    }

  let add_to_storage set =
    Cutils.update "BookEditor" (module RawState) @@ fun state ->
    {state with sets = state.sets @ [set]}

  let clear (editor : t) =
    Input.clear editor.elements.name;
    Input.clear editor.elements.date;
    Selector.clear editor.elements.sets

  let submit ~edit (editor : t) =
    match (S.value (state editor), edit) with
    | (None, _) -> lwt_none
    | (Some {name; date; sets}, id) ->
      Lwt.map some @@
        (
          match id with
          | None -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create)
          | Some id -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) id
        )
          (
            Model.Book.make
              ~title: name
              ?date
              ~contents: (List.map (fun set -> Model.Book.Set (set, Model.SetParameters.none)) sets)
              ()
          )
end

let create ?on_save ?text ?edit () =
  MainPage.assert_can_create @@ fun () ->
  let%lwt editor = Editor.create ~text ~edit in
  Page.make'
    ~title: (lwt @@ (match edit with None -> "Add" | Some _ -> "Edit") ^ " a book")
    ~on_load: (fun () -> Input.focus editor.elements.name)
    [Input.html editor.elements.name;
    Input.html editor.elements.date;
    Selector.render
      ~make_result: AnyResult.make_set_result'
      ~make_more_results: (fun set -> [Utils.ResultRow.(make [cell ~a: [a_colspan 9999] [Formatters.Set.tunes' set]])])
      ~field_name: "Sets"
      ~model_name: "set"
      ~create_dialog_content: (fun ?on_save text -> SetEditor.create ?on_save ~text ())
      editor.elements.sets;
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
