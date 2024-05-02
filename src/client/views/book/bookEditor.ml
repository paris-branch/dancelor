open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module Model = Dancelor_client_model
module SCDDB = Dancelor_common.SCDDB
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters

type ('name, 'date, 'sets) gen = {
  name : 'name;
  date : 'date;
  sets : 'sets;
}
[@@deriving yojson]

module RawState = struct
  (* Dirty trick to convince Yojson to serialise slugs. *)
  type set = Model.Set.t
  let set_to_yojson _ = assert false
  let set_of_yojson _ = assert false

  type t = (
    string,
    string,
    set Slug.t list
  ) gen
  [@@deriving yojson]

  let empty = {
    name = "";
    date = "";
    sets = [];
  }

  let _key = "BookEditor.RawState"
end

module Editor = struct
  type t = (
    string Input.Text.t,
    PartialDate.t option Input.Text.t,
    Model.Set.t ListSelector.t
  ) gen

  let raw_state (editor : t) : RawState.t S.t =
    S.bind (Input.Text.raw_signal editor.name) @@ fun name ->
    S.bind (Input.Text.raw_signal editor.date) @@ fun date ->
    S.bind (ListSelector.raw_signal editor.sets) @@ fun sets ->
    S.const { name; date; sets }

  let state (editor : t) =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal editor.name) @@ fun name ->
    RS.bind (Input.Text.signal editor.date) @@ fun date ->
    RS.bind (ListSelector.signal editor.sets) @@ fun sets ->
    RS.pure { name; date; sets }

  let create () : t =
    Utils.with_local_storage (module RawState) raw_state @@ fun initial_state ->
    let name = Input.Text.make initial_state.name @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let date = Input.Text.make initial_state.date @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string)
      % Option.of_string_nonempty
    in
    let sets = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Set.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Set.search ~threshold ~slice filter
          )
        ~serialise: Model.Set.slug
        ~unserialise: Model.Set.get
        initial_state.sets
    in
    {name; date; sets}

  let clear (editor : t) =
    Input.Text.clear editor.name;
    Input.Text.clear editor.date;
    ListSelector.clear editor.sets

  let submit (editor : t) =
    match S.value (state editor) with
    | None -> Lwt.return_none
    | Some {name; date; sets} ->
      Lwt.map Option.some @@
      Model.Book.make_and_save
        ~title: name
        ?date
        ~contents: (List.map (fun set -> Model.Book.Set (set, Model.SetParameters.none)) sets)
        ~modified_at: (Datetime.now ()) (* FIXME: optional argument *)
        ~created_at: (Datetime.now ()) (* FIXME: not even optional *)
        ()
end

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let refresh _ = ()
let contents t = t.content
let init t = refresh t

let createNewAPI ?on_save () =
  let editor = Editor.create () in
  div [
    h2 ~a:[a_class ["title"]] [txt "Add a book"];

    form [
      Input.Text.render
        editor.name
        ~label: "Name"
        ~placeholder: "eg. The Dusty Miller Book";
      Input.Text.render
        editor.date
        ~label: "Date of devising"
        ~placeholder: "eg. 2019 or 2012-03-14";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_set_result'
        ~field_name: "set"
        ~model_name: "set"
        ~create_dialog_content: SetEditor.createNewAPI
        editor.sets;

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (Editor.state editor))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (Editor.submit editor) @@ Option.iter @@ fun book ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_book (Model.Book.slug book))
              | Some on_save -> on_save book
            )
          ();
        Button.clear
          ~onclick: (fun () -> Editor.clear editor)
          ();
      ]
    ]
  ]

let create ?on_save page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  Lwt.async (fun () ->
      document##.title := Js.string "Add a book | Dancelor";
      Lwt.return ()
    );
  Dom.appendChild content (To_dom.of_div (createNewAPI ?on_save ()));
  {page; content}
