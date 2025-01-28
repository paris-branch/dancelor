open Nes
open Js_of_ocaml
open Components
open Html
open Utils
module SCDDB = Dancelor_common.SCDDB
module Endpoints = Dancelor_common.Endpoints

type ('name, 'date, 'sets) gen = {
  name: 'name;
  date: 'date;
  sets: 'sets;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type set = Model.Set.t
  let set_to_yojson _ = assert false
  let set_of_yojson _ = assert false

  type t =
    (string, string, (string * set Slug.t list)) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    date = "";
    sets = ("", []);
  }
end

module State = struct
  type t =
    (string, PartialDate.t option, Model.Set.t Dancelor_common.Entry.t list) gen

  let to_raw_state (state : t) : RawState.t = {
    name = state.name;
    date = Option.fold ~none: "" ~some: PartialDate.to_string state.date;
    sets = ("", List.map Dancelor_common.Entry.slug state.sets);
  }

  exception Non_convertible

  let of_model (book : Model.Book.t Dancelor_common.Entry.t) : t Lwt.t =
    let%lwt contents = Model.Book.contents book in
    let sets =
      List.map
        (function
          | Model.Book.Set (set, params) when params = Model.SetParameters.none -> set
          | _ -> raise Non_convertible
        )
        contents
    in
    Lwt.return
      {
        name = (Dancelor_common.Entry.value book).title;
        date = (Dancelor_common.Entry.value book).date;
        sets;
      }
end

module Editor = struct
  type t = {
    elements:
      (string Input.Text.t, PartialDate.t option Input.Text.t, (Selector.many, Model.Set.t) Selector.t) gen;
    set_interacted: unit -> unit;
  }

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.elements.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.elements.date) @@ fun date ->
    S.bind (Selector.raw_signal editor.elements.sets) @@ fun sets ->
    S.const {name; date; sets}

  let state (editor : t) : State.t option S.t =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.elements.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.elements.date) @@ fun date ->
    RS.bind (Selector.signal_many editor.elements.sets) @@ fun sets ->
    RS.pure {name; date; sets}

  let with_or_without_local_storage ~text ~edit f =
    match (text, edit) with
    | Some text, _ ->
      Lwt.return @@ f {RawState.empty with name = text}
    | _, Some slug ->
      let%lwt book = Model.Book.get slug in
      let%lwt raw_state = Lwt.map State.to_raw_state (State.of_model book) in
      Lwt.return @@ f raw_state
    | _, None ->
      Lwt.return @@
      Cutils.with_local_storage "BookEditor" (module RawState) raw_state f

  let create ~text ~edit : t Lwt.t =
    with_or_without_local_storage ~text ~edit @@ fun initial_state ->
    let (has_interacted, set_interacted) = S.create false in
    let set_interacted () = set_interacted true in
    let name =
      Input.Text.make ~has_interacted initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let date =
      Input.Text.make ~has_interacted initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string) %
      Option.of_string_nonempty
    in
    let sets =
      Selector.make
        ~arity: Selector.many
        ~search: (fun slice input ->
            let%rlwt filter = Lwt.return (Model.Set.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Set.search slice filter
          )
        ~serialise: Dancelor_common.Entry.slug
        ~unserialise: Model.Set.get
        initial_state.sets
    in
    {
      elements = {name; date; sets};
      set_interacted;
    }

  let add_to_storage set =
    Cutils.update "BookEditor" (module RawState) @@ fun state ->
    {state with sets = (fst state.sets, snd state.sets @ [set])}

  let clear (editor : t) =
    Input.Text.clear editor.elements.name;
    Input.Text.clear editor.elements.date;
    Selector.clear editor.elements.sets

  let submit ~edit (editor : t) =
    match (S.value (state editor), edit) with
    | (None, _) -> Lwt.return_none
    | (Some {name; date; sets}, slug) ->
      Lwt.map Option.some @@
      Model.Book.save ?slug @@
      Model.Book.make
        ~title: name
        ?date
        ~contents: (List.map (fun set -> Model.Book.Set (set, Model.SetParameters.none)) sets)
        ()
end

let create ?on_save ?text ?edit () =
  let title = (match edit with None -> "Add" | Some _ -> "Edit") ^ " a book" in
  Page.make ~title: (S.const title) @@
  L.div
    (
      try%lwt
        let%lwt editor = Editor.create ~text ~edit in
        Lwt.return @@
        [
          h2 ~a: [a_class ["title"]] [txt title];
          form
            [
              Input.Text.render
                editor.elements.name
                ~label: "Name"
                ~placeholder: "eg. The Dusty Miller Book";
              Input.Text.render
                editor.elements.date
                ~label: "Date of devising"
                ~placeholder: "eg. 2019 or 2012-03-14";
              Selector.render
                ~make_result: AnyResult.make_set_result'
                ~make_more_results: (fun set -> [Utils.ResultRow.(make [lcell ~a: [a_colspan 9999] (Formatters.Set.tunes set)])])
                ~field_name: ("Sets", "set")
                ~model_name: "set"
                ~create_dialog_content: (fun ?on_save text -> Page.get_content @@ SetEditor.create ?on_save ~text ())
                editor.elements.sets;
              Button.group
                [
                  Button.save
                    ~disabled: (S.map Option.is_none (Editor.state editor))
                    ~onclick: (fun () ->
                        editor.set_interacted ();
                        Fun.flip Lwt.map (Editor.submit ~edit editor) @@
                        Option.iter @@ fun book ->
                        Editor.clear editor;
                        match on_save with
                        | None -> Dom_html.window##.location##.href := Js.string (Endpoints.Page.href_book (Dancelor_common.Entry.slug book))
                        | Some on_save -> on_save book
                      )
                    ();
                  Button.clear
                    ~onclick: (fun () -> Editor.clear editor)
                    ();
                ]
            ]
        ]
      with
      | State.Non_convertible ->
        Lwt.return
          [
            h2 ~a: [a_class ["title"]] [txt "Error"];
            p [txt "This book cannot be edited."];
          ]
    )
