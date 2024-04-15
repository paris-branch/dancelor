open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module Model = Dancelor_client_model
module SCDDB = Dancelor_common.SCDDB
module PageRouter = Dancelor_common.PageRouter
open Dancelor_client_utils
module Formatters = Dancelor_client_formatters

module State = struct
  module Value = struct
    type t = {
      name : string;
      kind : Model.Kind.Dance.t;
      devisers : Model.Person.t list;
      date : PartialDate.t option;
      disambiguation : string option;
      two_chords : bool;
      scddb_id : SCDDB.entry_id option;
    }
  end

  type t = {
    name : string Input.Text.t;
    kind : Model.Kind.Dance.t Input.Text.t;
    devisers : Model.Person.t ListSelector.t;
    date : PartialDate.t option Input.Text.t;
    disambiguation : string option Input.Text.t;
    two_chords : (bool, string) Result.t Choices.t;
    scddb_id : SCDDB.entry_id option Input.Text.t;
  }

  let create () =
    let name = Input.Text.make "" @@
      Result.of_string_nonempty ~empty: "The name cannot be empty."
    in
    let kind = Input.Text.make "" @@
      Option.to_result ~none:"Not a valid kind" % Model.Kind.Dance.of_string_opt
    in
    let devisers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        Result.ok
    in
    let date = Input.Text.make "" @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string)
      % Option.of_string_nonempty
    in
    let disambiguation = Input.Text.make "" @@
      Result.ok % Option.of_string_nonempty
    in
    let two_chords = Choices.make_radios' [
        Choices.choice' ~value:false [txt "One chord"];
        Choices.choice' ~value:true [txt "Two chords"];
      ]
        ~validate: (Option.to_result ~none:"A choice must be made")
    in
    let scddb_id = Input.Text.make "" @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Dance)
      % Option.of_string_nonempty
    in
    {name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let clear state =
    Input.Text.clear state.name;
    Input.Text.clear state.kind;
    ListSelector.clear state.devisers;
    Input.Text.clear state.date;
    Input.Text.clear state.disambiguation;
    (* FIXME: clear two chords *)
    Input.Text.clear state.scddb_id

  let signal state =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal state.name) @@ fun name ->
    RS.bind (Input.Text.signal state.kind) @@ fun kind ->
    RS.bind (ListSelector.signal state.devisers) @@ fun devisers ->
    RS.bind (Input.Text.signal state.date) @@ fun date ->
    RS.bind (Input.Text.signal state.disambiguation) @@ fun disambiguation ->
    RS.bind (Choices.signal state.two_chords) @@ fun two_chords ->
    RS.bind (Input.Text.signal state.scddb_id) @@ fun scddb_id ->
    RS.pure Value.{name; kind; devisers; date; disambiguation; two_chords; scddb_id}

  let submit state =
    match S.value (signal state) with
    | None -> Lwt.return_none
    | Some Value.{name; kind; devisers; date; disambiguation; two_chords; scddb_id} ->
      Lwt.map Option.some @@
      Model.Dance.make_and_save
        ~name
        ~kind
        ~devisers
        ~two_chords
        ?scddb_id
        ?disambiguation
        ?date
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
  let state = State.create () in
  div [
    h2 ~a:[a_class ["title"]] [txt "Add a dance"];

    form [
      Input.Text.render state.name ~placeholder:"Name";
      Input.Text.render state.kind ~placeholder:"Kind (eg. 8x32R, 2x(16R+16S))";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: "deviser"
        ~model_name: "person"
        ~create_dialog_content: PersonEditor.createNewAPI
        state.devisers;
      Input.Text.render state.date ~placeholder:"Date of devising (eg. 2019 or 2012-03-14)";
      Choices.render state.two_chords;
      Input.Text.render state.scddb_id ~placeholder:"Strathspey database URI or id (optional)";
      Input.Text.render state.disambiguation ~placeholder:"Disambiguation";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (State.signal state))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (State.submit state) @@ Option.iter @@ fun dance ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_dance (Model.Dance.slug dance))
              | Some on_save -> on_save dance
            )
          ();
        Button.clear
          ~onclick: (fun () -> State.clear state)
          ();
      ]
    ]
  ]

let create ?on_save page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  Lwt.async (fun () ->
      document##.title := Js.string "Add a dance | Dancelor";
      Lwt.return ()
    );
  Dom.appendChild content (To_dom.of_div (createNewAPI ?on_save ()));
  {page; content}
