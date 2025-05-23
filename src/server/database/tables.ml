open NesUnix
open Common

module Source = Table.Make(struct
  include ModelBuilder.Core.Source

  let slug_hint source = Lwt.return source.name
  let separate_fields = []
  let dependencies _ = Lwt.return []
  let standalone = false
end)

module Person = Table.Make(struct
  include ModelBuilder.Core.Person

  let slug_hint person = Lwt.return person.name
  let separate_fields = []
  let dependencies _ = Lwt.return []
  let standalone = false
end)

module User = Table.Make(struct
  include ModelBuilder.Core.User

  let slug_hint user = Lwt.bind (Person.get user.person) (fun person -> Lwt.return (Entry.value person).name)
  let separate_fields = []
  let dependencies user =
    Lwt.return [
      Table.make_slug_and_table (module Person) (ModelBuilder.Core.User.person user)
    ]
  let standalone = true
end)

module Dance = Table.Make(struct
  include ModelBuilder.Core.Dance

  let slug_hint dance = Lwt.return dance.name
  let separate_fields = []
  let dependencies dance =
    Lwt.return
      (
        List.map (Table.make_slug_and_table (module Person)) (ModelBuilder.Core.Dance.devisers dance)
      )

  let standalone = true
end)

module Tune = Table.Make(struct
  include ModelBuilder.Core.Tune

  let slug_hint tune = Lwt.return tune.name
  let separate_fields = []
  let dependencies tune =
    Lwt.return
      (
        List.map (Table.make_slug_and_table (module Dance)) (ModelBuilder.Core.Tune.dances tune) @
          List.map (Table.make_slug_and_table (module Person)) (ModelBuilder.Core.Tune.composers tune)
      )

  let standalone = false
end)

module Version = Table.Make(struct
  include ModelBuilder.Core.Version

  let slug_hint version = Lwt.bind (Tune.get version.tune) (fun tune -> Lwt.return (Entry.value tune).name)
  let separate_fields = [("content", "content.ly")]
  let dependencies version =
    Lwt.return
      (
        [Table.make_slug_and_table (module Tune) (ModelBuilder.Core.Version.tune version)] @
        List.map (Table.make_slug_and_table (module Source)) (ModelBuilder.Core.Version.sources version) @
        List.map (Table.make_slug_and_table (module Person)) (ModelBuilder.Core.Version.arrangers version)
      )

  let standalone = true
end)

module SetModel = struct
  include ModelBuilder.Core.Set

  let slug_hint set = Lwt.return set.name
  let separate_fields = []
  let dependencies set =
    Lwt.return
      (
        List.map (Table.make_slug_and_table (module Version) % fst) (ModelBuilder.Core.Set.contents set) @
          List.map (Table.make_slug_and_table (module Person)) (ModelBuilder.Core.Set.conceptors set)
      )

  let standalone = true
end

module Set = Table.Make(SetModel)

module Book = Table.Make(struct
  include ModelBuilder.Core.Book

  let slug_hint book = Lwt.return book.title
  let separate_fields = []
  let dependencies book =
    let%lwt dependencies =
      Lwt_list.map_p
        (function
          | ModelBuilder.Core.Book.Page.Version (version, parameters) ->
            Lwt.return
              (
                [Table.make_slug_and_table (module Version) version] @
                  match ModelBuilder.Core.VersionParameters.for_dance parameters with
                  | None -> []
                  | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
          | ModelBuilder.Core.Book.Page.Set (set, parameters) ->
            Lwt.return
              (
                [Table.make_slug_and_table (module Set) set] @
                  match ModelBuilder.Core.SetParameters.for_dance parameters with
                  | None -> []
                  | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
          | ModelBuilder.Core.Book.Page.InlineSet (set, parameters) ->
            let%lwt set_dependencies = SetModel.dependencies set in
            Lwt.return
              (
                set_dependencies @
                  match ModelBuilder.Core.SetParameters.for_dance parameters with
                  | None -> []
                  | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
        )
        (ModelBuilder.Core.Book.contents book)
    in
    Lwt.return (List.flatten dependencies)

  let standalone = true
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

module Log = (val Logger.create "database": Logs.LOG)

module Initialise = struct
  let sync_db () =
    Log.info (fun m -> m "Syncing database changes");
    if (not !Config.init_only) && !Config.sync_storage then
      Storage.sync_changes ()
    else
      Lwt.return_unit

  let load_tables () =
    Log.info (fun m -> m "Loading tables");
    tables
    |> Lwt_list.iter_s @@ fun (module Table : Table.S) ->
      Table.load ()

  let check_dependency_problems () =
    Log.info (fun m -> m "Checking for dependency problems");
    let%lwt found_problem =
      Lwt_list.fold_left_s
        (fun found_problem (module Table : Table.S) ->
          let%lwt problems = Table.list_dependency_problems () in
          (
            problems
            |> List.iter @@ function
                | Error.DependencyDoesNotExist ((from_key, from_slug), (to_key, to_slug)) ->
                  Log.warn (fun m -> m "%s / %s refers to %s / %s that does not exist" from_key from_slug to_key to_slug)
                | DependencyViolatesStatus ((from_key, from_slug, from_status), (to_key, to_slug, to_status)) ->
                  Log.warn (fun m -> m "%s / %s [%a] refers to %s / %s [%a]" from_key from_slug Status.pp from_status to_key to_slug Status.pp to_status)
                | DependencyViolatesPrivacy ((from_key, from_slug, from_privacy), (to_key, to_slug, to_privacy)) ->
                  Log.warn (fun m -> m "%s / %s [%a] refers to %s / %s [%a]" from_key from_slug Privacy.pp from_privacy to_key to_slug Privacy.pp to_privacy)
                | _ -> ()
          );
          match found_problem, problems with
          | Some found_problem, _ -> Lwt.return_some found_problem
          | _, problem :: _ -> Lwt.return_some problem
          | _ -> Lwt.return_none
        )
        None
        tables
    in
    match found_problem with
    | None -> Lwt.return ()
    | Some problem -> Error.fail problem

  let report_without_accesses () =
    Log.info (fun m -> m "Checking for unaccessible entries");
    List.iter
      (fun (module Table : Table.S) ->
        if not Table.standalone then
          Table.report_without_accesses ()
      )
      tables;
    Lwt.return ()

  let initialise () =
    sync_db ();%lwt
    load_tables ();%lwt
    check_dependency_problems ();%lwt
    report_without_accesses ();%lwt
    Lwt.return_unit
end

let initialise = Initialise.initialise
