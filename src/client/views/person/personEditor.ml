open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module SCDDB = Dancelor_common.SCDDB
module Model = Dancelor_client_model
module PageRouter = Dancelor_common.PageRouter

module State = struct
  type t = {
    name : string Input.Text.t;
    scddb_id : SCDDB.entry_id option Input.Text.t;
  }

  let create () =
    let name = Input.Text.make @@ fun name ->
      if name = "" then Error "The name cannot be empty."
      else Ok name
    in
    let scddb_id = Input.Text.make @@ fun scddb_id ->
      if scddb_id = "" then
        Ok None
      else
        match int_of_string_opt scddb_id with
        | Some scddb_id -> Ok (Some scddb_id)
        | None ->
          match SCDDB.person_from_uri scddb_id with
          | Ok scddb_id -> Ok (Some scddb_id)
          | Error msg -> Error msg
    in
    {name; scddb_id}

  let clear state =
    state.name.set "";
    state.scddb_id.set ""

  let signal state =
    S.map Result.to_option @@
    RS.bind state.name.signal @@ fun name ->
    RS.bind state.scddb_id.signal @@ fun scddb_id ->
    RS.pure (name, scddb_id)

  let submit state =
    match S.value (signal state) with
    | None -> Lwt.return_none
    | Some (name, scddb_id) ->
      Lwt.map Option.some @@
      Model.Person.make_and_save
        ~name
        ?scddb_id
        ~modified_at: (Datetime.now ())
        ~created_at: (Datetime.now ())
        ()
end

type t =
  {
    page : Dancelor_client_elements.Page.t;
    content : Dom_html.divElement Js.t;
  }

let refresh _ = ()

let createNewAPI ?on_save () =
  let state = State.create () in
  div [
    h2 ~a:[a_class ["title"]] [txt "Add a person"];
    form [
      Input.Text.render state.name ~placeholder:"Name";
      Input.Text.render state.scddb_id ~placeholder:"Strathspey database URI or id (optional)";
      Button.group [
        Button.save
          ~disabled: (S.map Option.is_none (State.signal state))
          ~onclick: (fun () ->
              Fun.flip Lwt.map (State.submit state) @@ Option.iter @@ fun person ->
              match on_save with
              | None -> Dom_html.window##.location##.href := Js.string (PageRouter.path_person (Model.Person.slug person))
              | Some on_save -> on_save person
            )
          ();
        Button.clear
          ~onclick: (fun () -> State.clear state)
          ();
      ];
    ]
  ]

let create ?on_save page =
  let document = Dancelor_client_elements.Page.document page in
  let content = Dom_html.createDiv document in
  Lwt.async (fun () ->
      document##.title := Js.string ("Add a person | Dancelor");
      Lwt.return ()
    );
  Dom.appendChild content (To_dom.of_div (createNewAPI ?on_save ()));
  {page; content}

let contents t =
  t.content

let init t =
  refresh t
