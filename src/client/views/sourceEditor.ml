open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Input.prepare
    ~type_: Text
    ~label: "Name"
    ~placeholder: "eg. The Paris Book of Scottish Country Dances, volume 2"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Short name"
    ~placeholder: "eg. Paris Book 2"
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Star.prepare
    ~label: "Editors"
    (
      Selector.prepare
        ~label: "Editor"
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~unserialise: Model.Person.get
        ~make_result: AnyResult.make_person_result'
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 9999 or https://my.strathspey.org/dd/publication/9999/"
    ~serialise: (Option.fold ~none: "" ~some: string_of_int)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Publication) %
        Option.of_string_nonempty
    )
    () ^::
  Input.prepare
    ~type_: Textarea
    ~label: "Description"
    ~placeholder: "eg. Book provided by the RSCDS and containing almost all of the original tunes for the RSCDS dances. New editions come every now and then to add tunes for newly introduced RSCDS dances."
    ~serialise: (Option.value ~default: "")
    ~validate: (S.const % function "" -> Ok None | s -> Ok (Some s))
    () ^::
  nil

let submit (name, (short_name, (editors, (scddb_id, (description, ()))))) =
  Madge_client.call_exn Endpoints.Api.(route @@ Source Create) @@
    Model.Source.make ~name ~short_name ~editors ?scddb_id ?description ()

let break_down source =
  let%lwt editors = Model.Source.editors' source in
  lwt (
    Model.Source.name' source,
    (Model.Source.short_name' source, (editors, (Model.Source.scddb_id' source, (Model.Source.description' source, ()))))
  )

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "source"
    ~icon: "archive"
    editor
    ?on_save
    ?initial_text: text
    ~preview: Editor.no_preview
    ~submit
    ~break_down
    ~format: (Formatters.Source.name' ~link: true)
    ~href: (Endpoints.Page.href_source % Entry.id)
