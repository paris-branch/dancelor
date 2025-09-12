open Nes
open Common
open Components
open Html

let editor =
  let open Editor in
  Input.prepare_non_empty
    ~type_: Text
    ~label: "Name"
    ~placeholder: "eg. John Doe"
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/person/9999/"
    ~serialise: (Option.fold ~none: "" ~some: string_of_int)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Person) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let assemble (name, (scddb_id, ())) =
  Model.Person.make ~name ?scddb_id ()

let submit mode person =
  match mode with
  | Editor.Edit prev_person -> Madge_client.call_exn Endpoints.Api.(route @@ Person Update) (Entry.id prev_person) person
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Person Create) person

let unsubmit = lwt % Entry.value

let disassemble person =
  let name = Model.Person.name person in
  let scddb_id = Model.Person.scddb_id person in
  lwt (name, (scddb_id, ()))

let create mode =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "person"
    ~icon: "person"
    editor
    ~mode
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~format: (Formatters.Person.name' ~link: true)
    ~href: (Endpoints.Page.href_person % Entry.id)
