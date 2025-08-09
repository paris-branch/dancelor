open Nes
open Common

open Components
open Html
open Utils

(* FIXME: we lost editing capabilities. bring them back *)

(* let with_or_without_local_storage ~text ~edit f = *)
(*   match (text, edit) with *)
(*   | Some text, _ -> *)
(*     lwt @@ f {RawState.empty with name = text} *)
(*   | _, Some id -> *)
(*     let%lwt book = Model.Book.get id in *)
(*     let%lwt raw_state = State.to_raw_state <$> State.of_model book in *)
(*     lwt @@ f raw_state *)
(*   | _, None -> *)
(*     lwt @@ *)
(*       Cutils.with_local_storage "BookEditor" (module RawState) raw_state f *)

(* let create ~text ~edit : t Lwt.t = *)

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
                ~make_more_results: (fun set -> [Utils.ResultRow.(make [cell ~a: [a_colspan 9999] [Formatters.Set.tunes' set]])])
                ~label: "Set"
                ~model_name: "set"
                ~create_dialog_content: (fun ?on_save text -> SetEditor.create ?on_save ~text ())
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
                ~make_more_results: (fun version -> [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSvg.make version]]])
                ~label: "Version"
                ~model_name: "version"
                ~create_dialog_content: (fun ?on_save text -> VersionEditor.create ?on_save ~text ())
                ~search: (fun slice input ->
                  let%rlwt filter = lwt (Filter.Version.from_string input) in
                  ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
                )
                ~unserialise: Model.Version.get
                ()
            );
        ]
    ) ^::
  nil

let add_to_storage set =
  Editor.update_local_storage ~key: "book" editor @@ fun (name, (date, (contents, ()))) ->
  (name, (date, (contents @ [Some 0, [Left (Some set); Right None]], ())))

let submit _mode (title, (date, (contents, ()))) =
  let contents =
    List.map
      (function
        | Left set -> Model.Book.Set (set, Model.SetParameters.none)
        | Right version -> Model.Book.Version (version, Model.VersionParameters.none)
      )
      contents
  in
  let book = Model.Book.make ~title ?date ~contents () in
  Madge_client.call_exn Endpoints.Api.(route @@ Book Create) book

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

(*   let submit ~edit (editor : t) = *)
(*     match (S.value (state editor), edit) with *)
(*     | (None, _) -> lwt_none *)
(*     | (Some {name; date; contents}, id) -> *)
(*       let contents = *)
(*         List.map *)
(*           (function *)
(*             | `Set set -> Model.Book.Set (set, Model.SetParameters.none) *)
(*             | `Version version -> Model.Book.Version (version, Model.VersionParameters.none) *)
(*           ) *)
(*           contents *)
(*       in *)
(*       Lwt.map some @@ *)
(*         ( *)
(*           match id with *)
(*           | None -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create) *)
(*           | Some id -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) id *)
(*         ) *)
(*           (Model.Book.make ~title: name ?date ~contents ()) *)

(* ~title: (lwt @@ (match edit with None -> "Add" | Some _ -> "Edit") ^ " a book") *)

let create ?on_save ?text ?edit () =
  assert (edit = None);
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "book"
    ~icon: "book"
    editor
    ?on_save
    ~mode: (Option.fold ~none: Editor.CreateWithLocalStorage ~some: Editor.quickCreate text)
    ~format: (Formatters.Book.title_and_subtitle')
    ~href: (Endpoints.Page.href_book % Entry.id)
    ~preview: Editor.no_preview
    ~submit
    ~break_down
