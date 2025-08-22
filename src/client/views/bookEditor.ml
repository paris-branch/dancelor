open Nes
open Common

open Components
open Html
open Utils

let (show_preview, set_show_preview) = S.create false
let flip_show_preview () = set_show_preview (not (S.value show_preview))

let editor =
  let open Editor in
  Input.prepare
    ~type_: Text
    ~label: "Title"
    ~placeholder: "eg. The Dusty Miller Book"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Subtitle"
    ~placeholder: "eg. Twenty version of Dusty Miller in rainbow colours"
    ~serialise: Fun.id
    ~validate: (S.const % ok)
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Short title"
    ~placeholder: "eg. Dusty Miller"
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
    (
      let open Plus.TupleElt in
      Plus.prepare
        ~label: "Set or version"
        ~cast: (function
          | Zero (set, params) -> Model.Book.Set (set, params)
          | Succ Zero (version, params) -> Model.Book.Version (version, params)
          | _ -> assert false (* types guarantee this is not reachable *)
        )
        ~uncast: (function
          | Model.Book.Set (set, params) -> Zero (set, params)
          | Model.Book.Version (version, params) -> Succ (Zero (version, params))
        )
        (
          let open Plus.Bundle in
          Parameteriser.prepare
            (
              Selector.prepare
                ~make_result: AnyResult.make_set_result'
                ~make_more_results: (fun set ->
                  flip S.map show_preview @@ function
                    | true -> [Utils.ResultRow.(make [cell ~a: [a_colspan 9999] [Formatters.Set.tunes' set]])]
                    | false -> []
                )
                ~label: "Set"
                ~model_name: "set"
                ~create_dialog_content: SetEditor.create
                ~search: (fun slice input ->
                  let%rlwt filter = lwt (Filter.Set.from_string input) in
                  ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Search) slice filter
                )
                ~unserialise: Model.Set.get
                ()
            )
            SetParametersEditor.e ^::
          Parameteriser.prepare
            (
              Selector.prepare
                ~make_result: AnyResult.make_version_result'
                ~make_more_results: (fun version ->
                  flip S.map show_preview @@ function
                    | true -> [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSvg.make version]]]
                    | false -> []
                )
                ~label: "Version"
                ~model_name: "version"
                ~create_dialog_content: VersionEditor.create
                ~search: (fun slice input ->
                  let%rlwt filter = lwt (Filter.Version.from_string input) in
                  ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
                )
                ~unserialise: Model.Version.get
                ()
            )
            VersionParametersEditor.e ^::
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

let add_set_to_storage _set = assert false
(* let%lwt set_none = SetParametersEditor.empty_value () in *)
(* let%lwt version_none = VersionParametersEditor.empty_value () in *)
(* lwt @@ *)
(* Editor.update_local_storage ~key: "book" editor @@ fun (name, (date, (contents, ()))) -> *)
(* (name, (date, (contents @ [Some 0, [Left (Some set, set_none); Right (None, version_none)]], ()))) *)

let add_version_to_storage _version = assert false
(* let%lwt set_none = SetParametersEditor.empty_value () in *)
(* let%lwt version_none = VersionParametersEditor.empty_value () in *)
(* lwt @@ *)
(* Editor.update_local_storage ~key: "book" editor @@ fun (name, (date, (contents, ()))) -> *)
(* (name, (date, (contents @ [Some 1, [Left (None, set_none); Right (Some version, version_none)]], ()))) *)

let preview (title, (subtitle, (short_title, (authors, (date, (contents, (remark, (sources, (scddb_id, ()))))))))) =
  lwt_some @@ Model.Book.make ~title ~subtitle ~short_title ~authors ?date ~contents ~remark ~sources ?scddb_id ()

let submit mode book =
  match mode with
  | Editor.Edit prev_book -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) (Entry.id prev_book) book
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create) book

let break_down book =
  let title = Model.Book.title' book in
  let subtitle = Model.Book.subtitle' book in
  let short_title = Model.Book.short_title' book in
  let%lwt authors = Model.Book.authors' book in
  let date = Model.Book.date' book in
  let%lwt contents = Model.Book.contents' book in
  let remark = Model.Book.remark' book in
  let%lwt sources = Model.Book.sources' book in
  let scddb_id = Model.Book.scddb_id' book in
  lwt (title, (subtitle, (short_title, (authors, (date, (contents, (remark, (sources, (scddb_id, ())))))))))

let create mode =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "book"
    ~icon: "book"
    editor
    ~mode
    ~format: (Formatters.Book.title_and_subtitle')
    ~href: (Endpoints.Page.href_book % Entry.id)
    ~preview
    ~submit
    ~break_down
