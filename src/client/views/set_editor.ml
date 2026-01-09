open Nes
open Common
open Components
open Html
open Utils

let (show_preview, set_show_preview) = S.create false
let flip_show_preview () = set_show_preview (not (S.value show_preview))

type visibility' =
  | Owners_only
  | Everyone
  | Select_viewers of Model.User.entry NEList.t

let visibility'_to_visibility : visibility' -> Entry.Access.Private.visibility = function
  | Owners_only -> Owners_only
  | Everyone -> Everyone
  | Select_viewers users -> Select_viewers (NEList.map Entry.id users)

let visibility_to_visibility' : Entry.Access.Private.visibility -> visibility' Lwt.t = function
  | Owners_only -> lwt Owners_only
  | Everyone -> lwt Everyone
  | Select_viewers users ->
    let%lwt users = Monadise_lwt.monadise_1_1 NEList.map (Option.get <%> Model.User.get) users in
    lwt (Select_viewers users)

let editor user =
  let open Editor in
  Input.prepare_non_empty
    ~type_: Text
    ~label: "Name"
    ~placeholder: "eg. The Dusty Miller"
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
        ~make_descr: (lwt % NEString.to_string % Model.Person.name')
        ~make_result: (Any_result.make_person_result ?context: None)
        ~label: "Conceptor"
        ~model_name: "person"
        ~create_dialog_content: Person_editor.create
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
            ~make_descr: (Lwt.map NEString.to_string % Model.Version.one_name')
            ~make_result: (Any_result.make_version_result ?context: None)
            ~make_more_results: (fun version ->
              flip S.map show_preview @@ function
                | true -> [Result_row.make [Result_row.cell ~a: [a_colspan 9999] [Version_snippets.make ~show_audio: false version]]]
                | false -> []
            )
            ~label: "Version"
            ~model_name: "version"
            ~create_dialog_content: Version_editor.create
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Version.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
            )
            ~unserialise: Model.Version.get
            ()
        )
        (
          Version_parameters_editor.e
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
        | true -> [flip_show_preview_button ~icon: (Action Show)]
        | false -> [flip_show_preview_button ~icon: (Action Hide)]
    ) ^::
  Input.prepare
    ~type_: Text
    ~label: "Order"
    ~placeholder: "eg. 1,2,3,4,2,3,4,1"
    ~serialise: Model.Set_order.to_string
    ~validate: (
      S.const %
        Option.to_result ~none: "Not a valid order." %
        Model.Set_order.of_string_opt
    )
    () ^::
  Star.prepare_non_empty
    ~label: "Owners"
    ~empty: [user]
    (
      Selector.prepare
        ~label: "Owner"
        ~model_name: "user"
        ~make_descr: (lwt % NEString.to_string % Model.User.username')
        ~make_result: (Any_result.make_user_result ?context: None)
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.User.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ User Search) slice filter
        )
        ~unserialise: Model.User.get
        ()
    ) ^::
  (
    let open Plus.Bundle in
    let open Plus.Tuple_elt in
    Plus.prepare
      ~label: "Visibility"
      ~cast: (function
        | Zero() -> Owners_only
        | Succ Zero() -> Everyone
        | Succ Succ Zero viewers -> Select_viewers viewers
        | _ -> assert false (* types guarantee this is not reachable *)
      )
      ~uncast: (function
        | Owners_only -> Zero ()
        | Everyone -> one ()
        | Select_viewers viewers -> two viewers
      )
      (
        Nil.prepare ~label: "Owners only" () ^::
        Nil.prepare ~label: "Everyone" () ^::
        (
          Star.prepare_non_empty
            ~label: "Viewers"
            (
              Selector.prepare
                ~label: "Viewer"
                ~model_name: "user"
                ~make_descr: (lwt % NEString.to_string % Model.User.username')
                ~make_result: (Any_result.make_user_result ?context: None)
                ~search: (fun slice input ->
                  let%rlwt filter = lwt (Filter.User.from_string input) in
                  ok <$> Madge_client.call_exn Endpoints.Api.(route @@ User Search) slice filter
                )
                ~unserialise: Model.User.get
                ()
            )
        ) ^::
        nil
      )
  ) ^::
  nil

let assemble (name, (kind, (conceptors, (contents, (order, (owners, (visibility, ()))))))) = (
  Model.Set.make ~name ~kind ~conceptors ~contents ~order (),
  Entry.Access.Private.make ~owners: (NEList.map Entry.id owners) ~visibility: (visibility'_to_visibility visibility) ()
)

let submit mode (set, access) =
  match mode with
  | Editor.Edit prev_set -> Madge_client.call_exn Endpoints.Api.(route @@ Set Update) (Entry.id prev_set) set access
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Set Create) set access

let unsubmit entry =
  lwt (Entry.value entry, Entry.access entry)

let disassemble (set, access) =
  let name = Model.Set.name set in
  let kind = Model.Set.kind set in
  let%lwt conceptors = Model.Set.conceptors set in
  let%lwt contents = Model.Set.contents set in
  let order = Model.Set.order set in
  let%lwt owners = NEList.of_list_exn <$> Lwt_list.map_p (fun user -> Option.get <$> Model.User.get user) (NEList.to_list @@ Entry.Access.Private.owners access) in
  let%lwt visibility = visibility_to_visibility' @@ Entry.Access.Private.visibility access in
  lwt (name, (kind, (conceptors, (contents, (order, (owners, (visibility, ())))))))

let create mode =
  let%lwt user = Option.map Entry.id <$> Environment.user in
  Main_page.assert_can_create @@ fun () ->
  Editor.make_page
    ~key: "set"
    ~icon: (Model Set)
    ~mode
    (editor user)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~format: (Formatters.Set.name' ~link: true)
    ~href: (Endpoints.Page.href_set % Entry.id)
    ~check_product: (fun (set1, access1) (set2, access2) -> Model.Set.equal set1 set2 && Entry.Access.Private.equal access1 access2)
