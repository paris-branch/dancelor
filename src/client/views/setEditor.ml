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
    ~placeholder: "eg. The Dusty Miller"
    ~serialise: Fun.id
    ~validate: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. 8x32R or 2x(16R+16S)"
    ~serialise: Kind.Dance.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." %
        Kind.Dance.of_string_opt
    )
    () ^::
  Star.prepare
    ~label: "Conceptors"
    (
      Selector.prepare
        ~make_result: AnyResult.make_person_result'
        ~label: "Conceptor"
        ~model_name: "person"
        ~create_dialog_content: PersonEditor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Person.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Person Search) slice filter
        )
        ~unserialise: Model.Person.get
        ()
    ) ^::
  Star.prepare
    ~label: "Versions"
    (
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
        (
          VersionParametersEditor.e
        )
    )
    ~more_actions: (
      let flip_show_preview_button ~icon =
        Button.make
          ~classes: ["btn-info"]
          ~icon
          ~tooltip: "Toggle the preview of versions. This can take a lot of \
                     space on the page and is therefore disabled by default."
          ~onclick: (fun _ -> flip_show_preview (); lwt_unit)
          ()
      in
      flip S.map show_preview @@ function
        | true -> [flip_show_preview_button ~icon: "eye"]
        | false -> [flip_show_preview_button ~icon: "eye-slash"]
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Order"
    ~placeholder: "eg. 1,2,3,4,2,3,4,1"
    ~serialise: Model.SetOrder.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Not a valid order." %
        Model.SetOrder.of_string_opt
    )
    () ^::
  nil

let add_to_storage version =
  let%lwt none = VersionParametersEditor.empty_value () in
  lwt @@
  Editor.update_local_storage ~key: "set" editor @@ fun (name, (kind, (conceptors, (versions, (order, ()))))) ->
  (name, (kind, (conceptors, (versions @ [Some version, none], (order, ())))))

let preview (name, (kind, (conceptors, (contents, (order, ()))))) =
  lwt_some @@ Model.Set.make ~name ~kind ~conceptors ~contents ~order ()

let submit mode set =
  match mode with
  | Editor.Edit prev_set -> Madge_client.call_exn Endpoints.Api.(route @@ Set Update) (Entry.id prev_set) set
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Set Create) set

let break_down set =
  let name = Model.Set.name' set in
  let kind = Model.Set.kind' set in
  let%lwt conceptors = Model.Set.conceptors' set in
  let%lwt contents = Model.Set.contents' set in
  let order = Model.Set.order' set in
  lwt (name, (kind, (conceptors, (contents, (order, ())))))

let create mode =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "set"
    ~icon: "list-stars"
    ~mode
    editor
    ~preview
    ~submit
    ~break_down
    ~format: (Formatters.Set.name' ~link: true)
    ~href: (Endpoints.Page.href_set % Entry.id)
