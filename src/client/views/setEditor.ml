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
    ~placeholder: "eg. The Dusty Miller"
    ~validator: (S.const % Result.of_string_nonempty ~empty: "The name cannot be empty.")
    () ^::
  Input.prepare
    ~type_: Text
    ~label: "Kind"
    ~placeholder: "eg. 8x32R or 2x(16R+16S)"
    ~validator: (
      S.const %
        Option.to_result ~none: "Enter a valid kind, eg. 8x32R or 2x(16R+16S)." %
        Kind.Dance.of_string_opt
    )
    () ^::
  Star.prepare
    (
      Selector.prepare
        ~make_result: AnyResult.make_person_result'
        ~label: "Conceptor"
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
  Star.prepare
    (
      Selector.prepare
        ~make_result: AnyResult.make_version_result'
        ~make_more_results: (fun version ->
          [Utils.ResultRow.make [Utils.ResultRow.cell ~a: [a_colspan 9999] [VersionSvg.make version]]]
        )
        ~label: "Version"
        ~model_name: "version"
        ~create_dialog_content: (fun ?on_save text -> VersionEditor.create ?on_save ~text ())
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Version.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
        )
        ~serialise: Entry.id
        ~unserialise: Model.Version.get
        ()
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Order"
    ~placeholder: "eg. 1,2,3,4,2,3,4,1"
    ~validator: (
      S.const %
        Option.to_result ~none: "Not a valid order." %
        Model.SetOrder.of_string_opt
    )
    () ^::
  nil

let add_to_storage version =
  Editor.update_local_storage ~key: "set" editor @@ fun (name, (kind, (conceptors, (versions, (order, ()))))) ->
  (name, (kind, (conceptors, (versions @ [version], (order, ())))))

let submit (name, (kind, (conceptors, (versions, (order, ()))))) =
  Madge_client.call_exn Endpoints.Api.(route @@ Set Create) @@
    Model.Set.make
      ~name
      ~kind
      ~conceptors
      ~contents: (List.map (fun version -> (version, Model.VersionParameters.none)) versions)
      ~order
      ()

let create ?on_save ?text () =
  MainPage.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "set"
    ~icon: "list-stars"
    ?on_save
    ?initial_text: text
    editor
    ~preview: Editor.no_preview
    ~submit
    ~format: (Formatters.Set.name' ~link: true)
    ~href: (Endpoints.Page.href_set % Entry.id)
