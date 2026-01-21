open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Star.prepare_non_empty
    ~label: "Names"
    (
      Input.prepare_non_empty
        ~label: "Name"
        ~type_: Text
        ~placeholder: "eg. The Cairdin O't"
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. R or Strathspey"
    ~serialise: Kind.Base.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. R or Strathspey." %
        Kind.Base.of_string_opt
    )
    () ^::
  Star.prepare
    ~label: "Composer"
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Person.name')
        ~make_result: (Any_result.make_person_result ?context: None)
        ~results_when_no_search: (Option.to_list <$> Environment.person)
        ~label: "Composer"
        ~model_name: "person"
        ~create_dialog_content: Person_editor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~unserialise: Model.Person.get
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Date of composing"
    ~placeholder: "eg. 2019 or 2012-03-14"
    ~serialise: (Option.fold ~none: "" ~some: PartialDate.to_string)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % Option.to_result ~none: "Enter a valid date, eg. 2019 or 2012-03-14" % PartialDate.from_string) %
        Option.of_string_nonempty
    )
    () ^::
  Star.prepare
    ~label: "Dances"
    (
      Selector.prepare
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Dance.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
        )
        ~unserialise: Model.Dance.get
        ~make_descr: (lwt % NEString.to_string % Model.Dance.one_name')
        ~make_result: (Any_result.make_dance_result ?context: None)
        ~label: "Dance"
        ~model_name: "dance"
        ~create_dialog_content: Dance_editor.create
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Remark"
    ~placeholder: "Any additional information that doesn't fit in the other fields."
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/"
    ~serialise: (Option.fold ~none: "" ~some: string_of_int)
    ~validate: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Tune) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let assemble (names, (kind, (composers, (date, (dances, (remark, (scddb_id, ()))))))) =
  Model.Tune.make ~names ~kind ~composers ?date ~dances ~remark ?scddb_id ()

let submit mode tune =
  match mode with
  | Editor.Edit prev_tune -> Madge_client.call_exn Endpoints.Api.(route @@ Tune Update) (Entry.id prev_tune) tune
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Tune Create) tune

let unsubmit = lwt % Entry.value

let disassemble tune =
  let names = Model.Tune.names tune in
  let kind = Model.Tune.kind tune in
  let%lwt composers = Model.Tune.composers tune in
  let date = Model.Tune.date tune in
  let%lwt dances = Model.Tune.dances tune in
  let remark = Model.Tune.remark tune in
  let scddb_id = Model.Tune.scddb_id tune in
  lwt (names, (kind, (composers, (date, (dances, (remark, (scddb_id, ())))))))

let create mode =
  Main_page.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "tune"
    ~icon: (Model Tune)
    editor
    ~mode
    ~format: (Formatters.Tune.name' ~link: true)
    ~href: (Endpoints.Page.href_tune % Entry.id)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~check_product: Model.Tune.equal
