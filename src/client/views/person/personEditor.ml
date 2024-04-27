open Nes
open Js_of_ocaml
open Dancelor_client_components
open Dancelor_client_html
module SCDDB = Dancelor_common.SCDDB
module Model = Dancelor_client_model
module PageRouter = Dancelor_common.PageRouter

module Value = struct
  type t = {
    name : string;
    scddb_id : SCDDB.entry_id option;
  }
  [@@deriving yojson]

  let empty = {
    name = "";
    scddb_id = None;
  }

  let _key = "personEditor.value"
end

module State = struct
  type t = {
    name : string Input.Text.t;
    scddb_id : SCDDB.entry_id option Input.Text.t;
  }

  let signal state =
    S.map Result.to_option @@
    RS.bind (Input.Text.signal state.name) @@ fun name ->
    RS.bind (Input.Text.signal state.scddb_id) @@ fun scddb_id ->
    RS.pure Value.{name; scddb_id}

  let create () =
    Utils.with_local_storage (module Value) signal @@ fun initial_value ->
    let name = Input.Text.make initial_value.name @@
      Result.of_string_nonempty ~empty:"The name cannot be empty."
    in
    let scddb_id = Input.Text.make (Option.to_string @@ Option.map (Uri.to_string % SCDDB.person_uri) @@ initial_value.scddb_id) @@
      Option.fold
        ~none: (Ok None)
        ~some: (Result.map Option.some % SCDDB.entry_from_string SCDDB.Person)
      % Option.of_string_nonempty
    in
    {name; scddb_id}

  let clear state =
    Input.Text.clear state.name;
    Input.Text.clear state.scddb_id

  let submit state =
    match S.value (signal state) with
    | None -> Lwt.return_none
    | Some {name; scddb_id} ->
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
