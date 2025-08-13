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
    ~label: "Name"
    ~placeholder: "eg. The Dusty Miller Book"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Date of devising"
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
      Plus.prepare
        ~label: "Set or version"
        [
          Plus.wrap
            left
            Either.find_left
            left
            Either.find_left
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
            );
          Plus.wrap
            right
            Either.find_right
            right
            Either.find_right
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
            );
        ]
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
  nil

let add_set_to_storage set =
  Editor.update_local_storage ~key: "book" editor @@ fun (name, (date, (contents, ()))) ->
  (name, (date, (contents @ [Some 0, [Left (Some set); Right None]], ())))

let add_version_to_storage version =
  Editor.update_local_storage ~key: "book" editor @@ fun (name, (date, (contents, ()))) ->
  (name, (date, (contents @ [Some 1, [Left None; Right (Some version)]], ())))

let preview (title, (date, (contents, ()))) =
  let contents =
    List.map
      (function
        | Left set -> Model.Book.Set (set, Model.SetParameters.none)
        | Right version -> Model.Book.Version (version, Model.VersionParameters.none)
      )
      contents
  in
  lwt_some @@ Model.Book.make ~title ?date ~contents ()

let submit mode book =
  match mode with
  | Editor.Edit prev_book -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) (Entry.id prev_book) book
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create) book

let break_down book =
  let title = Model.Book.title' book in
  let date = Model.Book.date' book in
  let%lwt contents = Model.Book.contents' book in
  let contents =
    List.map
      (function
        | Model.Book.Set (set, params) when params = Model.SetParameters.none -> Left set
        | Model.Book.Version (version, params) when params = Model.VersionParameters.none -> Right version
        | _ -> raise Editor.NonConvertible
      )
      contents
  in
  lwt (title, (date, (contents, ())))

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
