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
  type t = {
    name : string Input.Text.t;
    kind : Model.Kind.Base.t Input.Text.t;
    composers : Model.Person.t ListSelector.t;
    date : PartialDate.t option Input.Text.t;
    dances : Model.Dance.t ListSelector.t;
    remark : string option Input.Text.t;
    scddb_id : SCDDB.entry_id option Input.Text.t;
  }

  let create () =
    let name = Input.Text.make @@ fun name ->
      if name = "" then Error "The name cannot be empty."
      else Ok name
    in
    let kind = Input.Text.make @@ fun kind ->
      match Model.Kind.Base.of_string_opt kind with
      | None -> Error "Not a valid kind"
      | Some kind -> Ok kind
    in
    let composers = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Person.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Person.search ~threshold ~slice filter
          )
        Result.ok
    in
    let date = Input.Text.make @@ fun date ->
      if date = "" then Ok None
      else
        try Ok (Some (PartialDate.from_string date))
        with _ -> Error "Not a valid date"
    in
    let dances = ListSelector.make
        ~search: (fun slice input ->
            let threshold = 0.4 in
            let%rlwt filter = Lwt.return (Model.Dance.Filter.from_string input) in
            Lwt.map Result.ok @@ Model.Dance.search ~threshold ~slice filter
          )
        Result.ok
    in
    let remark = Input.Text.make @@ fun remark ->
      Ok (if remark = "" then None else Some remark)
    in
    let scddb_id = Input.Text.make @@ fun scddb_id ->
      if scddb_id = "" then
        Ok None
      else
        match int_of_string_opt scddb_id with
        | Some scddb_id -> Ok (Some scddb_id)
        | None ->
          match SCDDB.dance_from_uri scddb_id with
          | Ok scddb_id -> Ok (Some scddb_id)
          | Error msg -> Error msg
    in
    {name; kind; composers; date; dances; remark; scddb_id}

  let clear state =
    state.name.set "";
    state.kind.set "";
    ListSelector.clear state.composers;
    state.date.set "";
    ListSelector.clear state.dances;
    state.remark.set "";
    state.scddb_id.set ""

  let signal state =
    S.map Result.to_option @@
    RS.bind state.name.signal @@ fun name ->
    RS.bind state.kind.signal @@ fun kind ->
    RS.bind (ListSelector.signal state.composers) @@ fun composers ->
    RS.bind state.date.signal @@ fun date ->
    RS.bind (ListSelector.signal state.dances) @@ fun dances ->
    RS.bind state.remark.signal @@ fun remark ->
    RS.bind state.scddb_id.signal @@ fun scddb_id ->
    RS.pure (name, kind, composers, date, dances, remark, scddb_id)

  let submit state =
    match S.value (signal state) with
    | None -> Lwt.return_none
    | Some (name, kind, composers, date, dances, remark, scddb_id) ->
      Lwt.map Option.some @@
      Model.Tune.make_and_save
        ~name
        ~kind
        ~composers
        ?date
        ~dances
        ?remark
        ?scddb_id
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
    h2 ~a:[a_class ["title"]] [txt "Add a tune"];

    form [
      Input.Text.render state.name ~placeholder:"Name";
      Input.Text.render state.kind ~placeholder:"Kind (eg. R, Strathspey)";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_person_result'
        ~field_name: "composer"
        ~model_name: "person"
        ~create_dialog_content: PersonEditor.createNewAPI
        state.composers;
      Input.Text.render state.date ~placeholder:"Date of devising (eg. 2019 or 2012-03-14)";
      ListSelector.render
        ~make_result: AnyResultNewAPI.make_dance_result'
        ~field_name: "dance"
        ~model_name: "dance"
        ~create_dialog_content: DanceEditor.createNewAPI
        state.dances;
      Input.Text.render state.remark ~placeholder:"Remark (optional)";
      Input.Text.render state.scddb_id ~placeholder:"Strathspey database URI or id (optional)";

      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (State.signal state))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (State.submit state) @@ Option.iter @@ fun dance ->
              let slug = Model.Tune.slug dance in
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_tune slug)
              | Some on_save -> on_save slug
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
      document##.title := Js.string "Add a tune | Dancelor";
      Lwt.return ()
    );
  Dom.appendChild content (To_dom.of_div (createNewAPI ?on_save ()));
  {page; content}
