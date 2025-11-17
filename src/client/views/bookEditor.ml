open Nes
open Common

open Components
open Html
open Utils

let (show_preview, set_show_preview) = S.create false
let flip_show_preview () = set_show_preview (not (S.value show_preview))

let versions_and_parameters ?(label = "Versions") () =
  Star.prepare_non_empty
    ~label: "Versions"
    (
      Parameteriser.prepare
        (
          Selector.prepare
            ~make_descr: (Lwt.map NEString.to_string % Model.Version.one_name')
            ~make_result: AnyResult.make_version_result'
            ~make_more_results: (fun version ->
              flip S.map show_preview @@ function
                | true -> [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSnippets.make ~show_audio: false version]]]
                | false -> []
            )
            ~label
            ~model_name: "version"
            ~create_dialog_content: VersionEditor.create
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Version.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
            )
            ~unserialise: Model.Version.get
            ()
        )
        VersionParametersEditor.e
    )

let set_and_parameters ?(label = "Set") () =
  Parameteriser.prepare
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Set.name')
        ~make_result: AnyResult.make_set_result'
        ~make_more_results: (fun set ->
          flip S.map show_preview @@ function
            | true -> [Utils.ResultRow.(make [cell ~a: [a_colspan 9999] [Formatters.Set.tunes' set]])]
            | false -> []
        )
        ~label
        ~model_name: "set"
        ~create_dialog_content: SetEditor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Set.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Search) slice filter
        )
        ~unserialise: Model.Set.get
        ()
    )
    SetParametersEditor.e

let dance_and_dance_page =
  let open Plus.Bundle in
  Cpair.prepare
    ~label: "Dance"
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Dance.one_name')
        ~make_result: AnyResult.make_dance_result'
        ~label: "Dance"
        ~model_name: "dance"
        ~create_dialog_content: DanceEditor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Dance.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
        )
        ~unserialise: Model.Dance.get
        ()
    )
    (
      let open Plus.TupleElt in
      Plus.prepare
        ~label: "Dance page"
        ~cast: (function
          | Zero() -> Model.Book.DanceOnly
          | Succ Zero versions_and_params -> Model.Book.DanceVersions versions_and_params
          | Succ Succ Zero (set, params) -> Model.Book.DanceSet (set, params)
          | _ -> assert false (* types guarantee this is not reachable *)
        )
        ~uncast: (function
          | Model.Book.DanceOnly -> Zero ()
          | Model.Book.DanceVersions versions_and_params -> one versions_and_params
          | Model.Book.DanceSet (set, params) -> two (set, params)
        )
        (
          Nil.prepare ~label: "Dance only" () ^::
          versions_and_parameters ~label: "+Versions" () ^::
          set_and_parameters ~label: "+Set" () ^::
          nil
        )
    )

let editor =
  let open Editor in
  Input.prepare_non_empty
    ~type_: Text
    ~label: "Title"
    ~placeholder: "eg. The Dusty Miller Book"
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
        ~make_result: AnyResult.make_person_result'
        ~model_name: "person"
        ~create_dialog_content: PersonEditor.create
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
  Star.prepare
    ~label: "Contents"
    ~make_header: (fun n -> div ~a: [a_class (if n = 0 then [] else ["pt-1"; "mt-1"; "border-top"])] [txtf "Page %d" (n + 1)])
    (
      let open Plus.TupleElt in
      Plus.prepare
        ~label: "Page"
        ~cast: (function
          | Zero title -> Model.Book.Part title
          | Succ Zero (dance, dance_page) -> Model.Book.Dance (dance, dance_page)
          | Succ Succ Zero versions_and_params -> Model.Book.Versions versions_and_params
          | Succ Succ Succ Zero (set, params) -> Model.Book.Set (set, params)
          | _ -> assert false (* types guarantee this is not reachable *)
        )
        ~uncast: (function
          | Model.Book.Part title -> Zero title
          | Model.Book.Dance (dance, dance_page) -> one (dance, dance_page)
          | Model.Book.Versions versions_and_params -> two versions_and_params
          | Model.Book.Set (set, params) -> three (set, params)
        )
        (
          let open Plus.Bundle in
          Input.prepare_non_empty
            ~type_: Text
            ~label: "Part"
            ~placeholder: "eg. Part CMXCVII"
            () ^::
          dance_and_dance_page ^::
          versions_and_parameters () ^::
          set_and_parameters () ^::
          nil
        )
    )
    ~more_actions: (
      let flip_show_preview_button ~icon =
        Button.make
          ~classes: ["btn-info"]
          ~icon
          ~tooltip: "Toggle the preview of sets and versions. This can take a \
                     lot of space on the page and is therefore disabled by \
                     default."
          ~onclick: (fun _ -> flip_show_preview (); lwt_unit)
          ()
      in
      flip S.map show_preview @@ function
        | true -> [flip_show_preview_button ~icon: "eye"]
        | false -> [flip_show_preview_button ~icon: "eye-slash"]
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Remark"
    ~placeholder: "eg. Dusty Miller"
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Star.prepare
    ~label: "Sources"
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Source.name')
        ~make_result: AnyResult.make_source_result'
        ~label: "Source"
        ~model_name: "source"
        ~create_dialog_content: SourceEditor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Source.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Source Search) slice filter
        )
        ~unserialise: Model.Source.get
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
  nil

let assemble (title, (authors, (date, (contents, (remark, (sources, (scddb_id, ()))))))) =
  Model.Book.make ~title ~authors ?date ~contents ~remark ~sources ?scddb_id ()

let submit mode book =
  match mode with
  | Editor.Edit prev_book -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) (Entry.id prev_book) book
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create) book

let unsubmit = lwt % Entry.value

let disassemble book =
  let title = Model.Book.title book in
  let%lwt authors = Model.Book.authors book in
  let date = Model.Book.date book in
  let%lwt contents = Model.Book.contents book in
  let remark = Model.Book.remark book in
  let%lwt sources = Model.Book.sources book in
  let scddb_id = Model.Book.scddb_id book in
  lwt (title, (authors, (date, (contents, (remark, (sources, (scddb_id, ())))))))

let create mode =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "book"
    ~icon: "book"
    editor
    ~mode
    ~format: Formatters.Book.title'
    ~href: (Endpoints.Page.href_book % Entry.id)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~check_product: Model.Book.equal
