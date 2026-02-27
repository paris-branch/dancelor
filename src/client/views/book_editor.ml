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

let versions_and_parameters ?(label = "Versions") () =
  Star.prepare_non_empty
    ~label: "Versions"
    (
      Parameteriser.prepare
        (
          Selector.prepare
            ~make_descr: (Lwt.map NEString.to_string % Model.Version.one_name')
            ~make_result: (Any_result.make_version_result ?context: None)
            ~make_more_results: (fun version ->
              S.flip_map show_preview @@ function
                | true -> [tr [td ~a: [a_colspan 9999] [Version_snippets.make ~show_audio: false version]]]
                | false -> []
            )
            ~label
            ~model_name: "version"
            ~create_dialog_content: Version_editor.create
            ~search: (fun slice input ->
              let%rlwt filter = lwt (Filter.Version.from_string input) in
              ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Version Search) slice filter
            )
            ~unserialise: Model.Version.get
            ()
        )
        Version_parameters_editor.e
    )

let set_and_parameters ?(label = "Set") () =
  Parameteriser.prepare
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Set.name')
        ~make_result: (Any_result.make_set_result ?context: None ?params: None)
        ~make_more_results: (fun set ->
          S.flip_map show_preview @@ function
            | true -> [tr [td ~a: [a_colspan 9999] [Formatters.Set.tunes' set]]]
            | false -> []
        )
        ~label
        ~model_name: "set"
        ~create_dialog_content: Set_editor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Set.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Set Search) slice filter
        )
        ~unserialise: Model.Set.get
        ()
    )
    Set_parameters_editor.e

let dance_and_dance_page =
  let open Plus.Bundle in
  Cpair.prepare
    ~label: "Dance"
    (
      Selector.prepare
        ~make_descr: (lwt % NEString.to_string % Model.Dance.one_name')
        ~make_result: (Any_result.make_dance_result ?context: None)
        ~label: "Dance"
        ~model_name: "dance"
        ~create_dialog_content: Dance_editor.create
        ~search: (fun slice input ->
          let%rlwt filter = lwt (Filter.Dance.from_string input) in
          ok <$> Madge_client.call_exn Endpoints.Api.(route @@ Dance Search) slice filter
        )
        ~unserialise: Model.Dance.get
        ()
    )
    (
      let open Plus.Tuple_elt in
      Plus.prepare
        ~label: "Dance page"
        ~cast: (function
          | Zero() -> Model.Book.Dance_only
          | Succ Zero versions_and_params -> Model.Book.Dance_versions versions_and_params
          | Succ Succ Zero (set, params) -> Model.Book.Dance_set (set, params)
          | _ -> assert false (* types guarantee this is not reachable *)
        )
        ~uncast: (function
          | Model.Book.Dance_only -> Zero ()
          | Model.Book.Dance_versions versions_and_params -> one versions_and_params
          | Model.Book.Dance_set (set, params) -> two (set, params)
        )
        ~selected_when_empty: 0
        (
          Nil.prepare ~label: "Dance only" () ^::
          versions_and_parameters ~label: "+Versions" () ^::
          set_and_parameters ~label: "+Set" () ^::
          nil
        )
    )

let editor user =
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
  Star.prepare
    ~label: "Contents"
    ~make_header: (fun n -> div ~a: [a_class (if n = 0 then [] else ["pt-1"; "mt-1"; "border-top"])] [txtf "Page %d" (n + 1)])
    (
      let open Plus.Tuple_elt in
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
      S.flip_map show_preview @@ function
        | true -> [flip_show_preview_button ~icon: (Action Show)]
        | false -> [flip_show_preview_button ~icon: (Action Hide)]
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
        ~make_result: (Any_result.make_source_result ?context: None)
        ~label: "Source"
        ~model_name: "source"
        ~create_dialog_content: Source_editor.create
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
  Star.prepare_non_empty
    ~label: "Owners"
    ~empty: [user]
    (
      Selector.prepare
        ~label: "Owner"
        ~model_name: "user"
        ~make_descr: (lwt % Model.User.Username.to_string % Model.User.username')
        ~make_result: (Any_result.make_user_result ?context: None)
        ~results_when_no_search: (Option.to_list <$> Environment.user)
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
      ~selected_when_empty: 0
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
                ~make_descr: (lwt % Model.User.Username.to_string % Model.User.username')
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

let assemble (title, (authors, (date, (contents, (remark, (sources, (scddb_id, (owners, (visibility, ()))))))))) = (
  Model.Book.make ~title ~authors ?date ~contents ~remark ~sources ?scddb_id (),
  Entry.Access.Private.make ~owners: (NEList.map Entry.id owners) ~visibility: (visibility'_to_visibility visibility) ()
)

let submit mode (book, access) =
  match mode with
  | Editor.Edit prev_book -> Madge_client.call_exn Endpoints.Api.(route @@ Book Update) (Entry.id prev_book) book access
  | _ -> Madge_client.call_exn Endpoints.Api.(route @@ Book Create) book access

let unsubmit entry =
  lwt (Entry.value entry, Entry.access entry)

let disassemble (book, access) =
  let title = Model.Book.title book in
  let%lwt authors = Model.Book.authors book in
  let date = Model.Book.date book in
  let%lwt contents = Model.Book.contents book in
  let remark = Model.Book.remark book in
  let%lwt sources = Model.Book.sources book in
  let scddb_id = Model.Book.scddb_id book in
  let%lwt owners = NEList.of_list_exn <$> Lwt_list.map_p (fun user -> Option.get <$> Model.User.get user) (NEList.to_list @@ Entry.Access.Private.owners access) in
  let%lwt visibility = visibility_to_visibility' @@ Entry.Access.Private.visibility access in
  lwt (title, (authors, (date, (contents, (remark, (sources, (scddb_id, (owners, (visibility, ())))))))))

let create mode =
  let%lwt user = Option.map Entry.id <$> Environment.user in
  (* FIXME: if [mode] is an edition, then we should assert_can_update_private *)
  Main_page.assert_can_create_private @@ fun () ->
  Editor.make_page
    ~key: "book"
    ~icon: (Model Book)
    (editor user)
    ~mode
    ~format: Formatters.Book.title'
    ~href: (Endpoints.Page.href_book % Entry.id)
    ~assemble
    ~submit
    ~unsubmit
    ~disassemble
    ~check_product: (fun (book1, access1) (book2, access2) -> Model.Book.equal book1 book2 && Entry.Access.Private.equal access1 access2)

let add () =
  create Create_with_local_storage

let edit book_id =
  let%lwt book = Option.get <$> Model.Book.get book_id in
  create (Edit book)
