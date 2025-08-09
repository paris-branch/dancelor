open Nes
open Common
open Components
open Html

let editor =
  let open Editor in
  Input.prepare
    ~type_: Text
    ~label: "Name"
    ~placeholder: "eg. John Doe"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
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

let submit _mode (name, (scddb_id, ())) =
  Madge_client.call_exn Endpoints.Api.(route @@ Person Create) @@
    Model.Person.make ~name ?scddb_id ()

let break_down person =
  lwt (
    Model.Person.name' person,
    (Model.Person.scddb_id' person, ())
  )

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "person"
    ~icon: "person"
    editor
    ?on_save
    ~mode: (Option.fold ~none: Editor.CreateWithLocalStorage ~some: Editor.quickCreate text)
    ~preview: Editor.no_preview
    ~submit
    ~break_down
    ~format: (Formatters.Person.name' ~link: true)
    ~href: (Endpoints.Page.href_person % Entry.id)
