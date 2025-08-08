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
    ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/person/9999/"
    ~validator: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Person) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "person"
    ~icon: "person"
    editor
    ?on_save
    ?initial_text: text
    ~preview: Editor.no_preview
    ~submit: (fun (name, (scddb_id, ())) ->
      Madge_client.call_exn Endpoints.Api.(route @@ Person Create) @@
        Model.Person.make ~name ?scddb_id ()
    )
    ~format: (Formatters.Person.name' ~link: true)
    ~href: (Endpoints.Page.href_person % Entry.id)
