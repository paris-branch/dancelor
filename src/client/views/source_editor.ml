open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Input.prepare_non_empty
    ~type_: Text
    ~label: "Name"
    ~placeholder: "eg. The Paris Book of Scottish Country Dances, volume 2"
    () ^::
  Input.prepare_option
    ~type_: Text
    ~label: "Short name"
    ~placeholder: "eg. Paris Book 2"
    ~serialise: id
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
        ~make_descr: (lwt % NEString.to_string % Model.Person.name')
        ~make_result: (Any_result.make_person_result ?context: None)
        ~results_when_no_search: (Option.to_list <$> Environment.person)
        ~model_name: "person"
        ~create_dialog_content: Person_editor.create
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Date of publication"
    ~placeholder: "eg. 2019 or 2012-03-14"
    ~serialise: (Option.fold ~none: "" ~some: PartialDate.to_string)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % Option.to_result ~none: "Not a valid date" % PartialDate.from_string) %
        Option.of_string_nonempty
    )
    () ^::
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
    ~type_: (Textarea {rows = 10})
    ~label: "Description"
    ~placeholder: "eg. Book provided by the RSCDS and containing almost all of the original tunes for the RSCDS dances. New editions come every now and then to add tunes for newly introduced RSCDS dances."
    ~serialise: (Option.value ~default: "")
    ~validate: (S.const % function "" -> Ok None | s -> Ok (Some s))
    () ^::
  nil

let assemble (name, (short_name, (editors, (date, (scddb_id, (description, ())))))) =
  Model.Source.make ~name ?short_name ~editors ?scddb_id ?description ?date ()

let submit mode source =
  match mode with
  | Editor.Edit prev_source -> Madge_client.call_exn Endpoints.Api.(route @@ Source Update) (Entry.id prev_source) source
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Source Create) source

let unsubmit = lwt % Entry.value

let disassemble source =
  let name = Model.Source.name source in
  let short_name = Model.Source.short_name source in
  let%lwt editors = Model.Source.editors source in
  let date = Model.Source.date source in
  let scddb_id = Model.Source.scddb_id source in
  let description = Model.Source.description source in
  lwt (name, (short_name, (editors, (date, (scddb_id, (description, ()))))))

let create mode =
  (* FIXME: if [mode] is an edition, then we should assert_can_update_public *)
  Main_page.assert_can_create_public @@ fun () ->
  Editor.make_page
    ~key: "source"
    ~icon: (Model Source)
    editor
    ~mode
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~check_product: Model.Source.equal
    ~format: (Formatters.Source.name' ~link: true)
    ~href: (Endpoints.Page.href_source % Entry.id)

let add () =
  create Create_with_local_storage

let edit id =
  let%lwt source = Option.get <$> Model.Source.get id in
  create (Edit source)
