open Nes
open Common

open Components
open Html
open Utils

let editor =
  let open Editor in
  Star.prepare_non_empty
    (
      Input.prepare
        ~label: "Name"
        ~type_: Text
        ~placeholder: "eg. The Cairdin O't"
        ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. R or Strathspey"
    ~validator: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. R or Strathspey." %
        Kind.Base.of_string_opt
    )
    () ^::
  Star.prepare
    (
      Selector.prepare
        ~make_result: AnyResult.make_person_result'
        ~label: "Composer"
        ~model_name: "person"
        ~create_dialog_content: (fun ?on_save text -> PersonEditor.create ?on_save ~text ())
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Person.get
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Date of devising"
    ~placeholder: "eg. 2019 or 2012-03-14"
    ~validator: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % Option.to_result ~none: "Enter a valid date, eg. 2019 or 2012-03-14" % PartialDate.from_string) %
        Option.of_string_nonempty
    )
    () ^::
  Star.prepare
    (
      Selector.prepare
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Dance.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Dance.get
        ~make_result: AnyResult.make_dance_result'
        ~label: "Dance"
        ~model_name: "dance"
        ~create_dialog_content: (fun ?on_save text -> DanceEditor.create ?on_save ~text ())
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Remark"
    ~placeholder: "Any additional information that doesn't fit in the other fields."
    ~validator: (S.const % ok % Option.of_string_nonempty)
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "SCDDB ID"
    ~placeholder: "eg. 2423 or https://my.strathspey.org/dd/tune/2423/"
    ~validator: (
      S.const %
        Option.fold
          ~none: (Ok None)
          ~some: (Result.map some % SCDDB.entry_from_string SCDDB.Tune) %
        Option.of_string_nonempty
    )
    () ^::
  nil

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "tune"
    ~icon: "music-note-list"
    editor
    ?on_save
    ?initial_text: text
    ~preview: Editor.no_preview
    ~submit: (fun (names, (kind, (composers, (date, (dances, (remark, (scddb_id, ()))))))) ->
      Madge_client.call_exn Endpoints.Api.(route @@ Tune Create) @@
        Model.Tune.make ~names ~kind ~composers ?date ~dances ?remark ?scddb_id ()
    )
    ~format: (Formatters.Tune.name' ~link: true)
    ~href: (Endpoints.Page.href_tune % Entry.id)
