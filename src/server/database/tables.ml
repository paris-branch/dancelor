open NesUnix
open Common

let id_for key entry : string * unit Entry.Id.t = (key, Entry.Id.unsafe_coerce entry)

module User = Table.Make(struct
  include Model_builder.Core.User
  let dependencies _ = []
  let wrap_any = Model_builder.Core.Any.user
end)

module Person = Table.Make(struct
  include Model_builder.Core.Person
  let dependencies person =
    List.map (id_for "user") (Option.to_list @@ Model_builder.Core.Person.user person)
  let wrap_any = Model_builder.Core.Any.person
end)

module Source = Table.Make(struct
  include Model_builder.Core.Source
  let dependencies source =
    List.map (id_for "person") (Model_builder.Core.Source.editors source)
  let wrap_any = Model_builder.Core.Any.source
end)

module Dance = Table.Make(struct
  include Model_builder.Core.Dance
  let dependencies dance =
    List.map (id_for "person") (Model_builder.Core.Dance.devisers dance)
  let wrap_any = Model_builder.Core.Any.dance
end)

module Tune = Table.Make(struct
  include Model_builder.Core.Tune
  let dependencies tune =
    List.map (id_for "dance") (Model_builder.Core.Tune.dances tune) @
      List.map (id_for "person" % (fun ({composer; _}: Model_builder.Core.Tune.composer_core) -> composer)) (Model_builder.Core.Tune.composers tune)
  let wrap_any = Model_builder.Core.Any.tune
end)

module Version = Table.Make(struct
  include Model_builder.Core.Version
  let dependencies version =
    [id_for "tune" (Model_builder.Core.Version.tune version)] @
    List.map (id_for "source" % (fun ({source; _}: Model_builder.Core.Version.source_core) -> source)) (Model_builder.Core.Version.sources version) @
    List.map (id_for "person") (Model_builder.Core.Version.arrangers version)
  let wrap_any = Model_builder.Core.Any.version
end)

module Set_model = struct
  include Model_builder.Core.Set
  let dependencies set =
    List.map (id_for "version" % fst) (Model_builder.Core.Set.contents set) @
    List.map (id_for "person") (Model_builder.Core.Set.conceptors set) @
    List.map (id_for "dance") (Model_builder.Core.Set.dances set)
  let wrap_any = Model_builder.Core.Any.set
end

module Set = Table.Make(Set_model)

module Book = Table.Make(struct
  include Model_builder.Core.Book
  let dependencies book =
    let contents_dependencies =
      List.map
        (function
          | Model_builder.Core.Book.Page.Part _ -> []
          | Model_builder.Core.Book.Page.Dance (dance, page_dance) ->
            let page_dance_dependencies =
              match page_dance with
              | Model_builder.Core.Book.Page.Dance_only -> []
              | Model_builder.Core.Book.Page.Dance_versions versions_and_params ->
                List.map (id_for "version" % fst) (NEList.to_list versions_and_params)
              | Model_builder.Core.Book.Page.Dance_set (set, _) ->
                [id_for "set" set]
            in
              (id_for "dance" dance :: page_dance_dependencies)
          | Model_builder.Core.Book.Page.Versions versions_and_params ->
            List.map (id_for "version" % fst) (NEList.to_list versions_and_params)
          | Model_builder.Core.Book.Page.Set (set, _) ->
            [id_for "set" set]
        )
        (Model_builder.Core.Book.contents book)
    in
    List.map (id_for "source") (Model_builder.Core.Book.sources book) @
    List.map (id_for "person") (Model_builder.Core.Book.authors book) @
    List.flatten contents_dependencies
  let wrap_any = Model_builder.Core.Any.book
end)

module Storage = Storage

let tables : (module Table.S)list = [
  (module Source);
  (module Person);
  (module User);
  (module Dance);
  (module Version);
  (module Tune);
  (module Set);
  (module Book)
]

let reverse_dependencies_of (id : 'any Entry.Id.t) : Table.reverse_dependencies =
  let id = Entry.Id.unsafe_coerce id in
  Reverse_dependencies (
    List.concat_map
      (fun (module T : Table.S) ->
        List.of_seq @@
          Seq.filter_map
            (fun entry ->
              if List.mem id (T.dependencies (Entry.value entry)) then
                Some (T._key, Entry.Id.unsafe_coerce @@ Entry.id entry)
              else None
            )
            (T.get_all ())
      )
      tables
  )

module Log = (val Logger.create "database": Logs.LOG)

module Initialise = struct
  let sync_db () =
    Log.info (fun m -> m "Syncing database changes");
    if (not !Config.init_only) && !Config.sync_storage then
      Storage.sync_changes ()
    else
      lwt_unit

  let load_tables () =
    Log.info (fun m -> m "Loading tables");
    tables
    |> Lwt_list.iter_s @@ fun (module Table : Table.S) ->
      Table.load ()

  let check_dependency_problems () =
    Log.info (fun m -> m "Checking for dependency problems");
    let found_problem =
      List.fold_left
        (fun found_problem (module Table : Table.S) ->
          let problems = Table.list_dependency_problems () in
          (
            problems
            |> List.iter @@ function
                | Error.Dependency_does_not_exist ((from_key, from_id), (to_key, to_id)) ->
                  Log.warn (fun m -> m "%s / %s refers to %s / %s that does not exist" from_key from_id to_key to_id)
                | _ -> ()
          );
          match found_problem, problems with
          | Some found_problem, _ -> Some found_problem
          | _, problem :: _ -> Some problem
          | _ -> None
        )
        None
        tables
    in
    match found_problem with
    | None -> ()
    | Some problem -> Error.fail problem

  let initialise () =
    sync_db ();%lwt
    load_tables ();%lwt
    check_dependency_problems ();
    lwt_unit
end

let initialise = Initialise.initialise
