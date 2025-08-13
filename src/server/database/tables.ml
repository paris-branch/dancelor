open NesUnix
open Common

module Source = Table.Make(struct
  include ModelBuilder.Core.Source

  let separate_fields = []
  let dependencies _ = lwt []
  let standalone = false
  let wrap_any = ModelBuilder.Core.Any.source
end)

module Person = Table.Make(struct
  include ModelBuilder.Core.Person

  let separate_fields = []
  let dependencies _ = lwt []
  let standalone = false
  let wrap_any = ModelBuilder.Core.Any.person
end)

module User = Table.Make(struct
  include ModelBuilder.Core.User

  let separate_fields = []
  let dependencies user =
    lwt [
      Table.make_id_and_table (module Person) (ModelBuilder.Core.User.person user)
    ]
  let standalone = true
  let wrap_any = ModelBuilder.Core.Any.user
end)

module Dance = Table.Make(struct
  include ModelBuilder.Core.Dance

  let separate_fields = []
  let dependencies dance =
    lwt
      (
        List.map (Table.make_id_and_table (module Person)) (ModelBuilder.Core.Dance.devisers dance)
      )
  let standalone = true
  let wrap_any = ModelBuilder.Core.Any.dance
end)

module Tune = Table.Make(struct
  include ModelBuilder.Core.Tune

  let separate_fields = []
  let dependencies tune =
    lwt
      (
        List.map (Table.make_id_and_table (module Dance)) (ModelBuilder.Core.Tune.dances tune) @
          List.map (Table.make_id_and_table (module Person)) (ModelBuilder.Core.Tune.composers tune)
      )

  let standalone = false
  let wrap_any = ModelBuilder.Core.Any.tune
end)

module Version = Table.Make(struct
  include ModelBuilder.Core.Version

  let separate_fields = [("content", "content.ly")]
  let dependencies version =
    lwt
      (
        [Table.make_id_and_table (module Tune) (ModelBuilder.Core.Version.tune version)] @
        List.map (Table.make_id_and_table (module Source)) (ModelBuilder.Core.Version.sources version) @
        List.map (Table.make_id_and_table (module Person)) (ModelBuilder.Core.Version.arrangers version)
      )

  let standalone = true
  let wrap_any = ModelBuilder.Core.Any.version
end)

module SetModel = struct
  include ModelBuilder.Core.Set

  let separate_fields = []
  let dependencies set =
    lwt
      (
        List.map (Table.make_id_and_table (module Version) % fst) (ModelBuilder.Core.Set.contents set) @
          List.map (Table.make_id_and_table (module Person)) (ModelBuilder.Core.Set.conceptors set)
      )

  let standalone = true
  let wrap_any = ModelBuilder.Core.Any.set
end

module Set = Table.Make(SetModel)

module Book = Table.Make(struct
  include ModelBuilder.Core.Book

  let separate_fields = []
  let dependencies book =
    let%lwt dependencies =
      Lwt_list.map_p
        (function
          | ModelBuilder.Core.Book.Page.Version (version, parameters) ->
            lwt
              (
                [Table.make_id_and_table (module Version) version] @
                  match ModelBuilder.Core.VersionParameters.for_dance parameters with
                  | None -> []
                  | Some dance -> [Table.make_id_and_table (module Dance) dance]
              )
          | ModelBuilder.Core.Book.Page.Set (set, parameters) ->
            lwt
              (
                [Table.make_id_and_table (module Set) set] @
                  match ModelBuilder.Core.SetParameters.for_dance parameters with
                  | None -> []
                  | Some dance -> [Table.make_id_and_table (module Dance) dance]
              )
        )
        (ModelBuilder.Core.Book.contents book)
    in
    lwt (List.flatten dependencies)

  let standalone = true
  let wrap_any = ModelBuilder.Core.Any.book
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
      lwt_unit

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
                | Error.DependencyDoesNotExist ((from_key, from_id), (to_key, to_id)) ->
                  Log.warn (fun m -> m "%s / %s refers to %s / %s that does not exist" from_key from_id to_key to_id)
                | DependencyViolatesStatus ((from_key, from_id, from_status), (to_key, to_id, to_status)) ->
                  Log.warn (fun m -> m "%s / %s [%a] refers to %s / %s [%a]" from_key from_id Status.pp from_status to_key to_id Status.pp to_status)
                | DependencyViolatesPrivacy ((from_key, from_id, from_privacy), (to_key, to_id, to_privacy)) ->
                  Log.warn (fun m -> m "%s / %s [%a] refers to %s / %s [%a]" from_key from_id Privacy.pp from_privacy to_key to_id Privacy.pp to_privacy)
                | _ -> ()
          );
          match found_problem, problems with
          | Some found_problem, _ -> lwt_some found_problem
          | _, problem :: _ -> lwt_some problem
          | _ -> lwt_none
        )
        None
        tables
    in
    match found_problem with
    | None -> lwt_unit
    | Some problem -> Error.fail problem

  let report_without_accesses () =
    Log.info (fun m -> m "Checking for unaccessible entries");
    List.iter
      (fun (module Table : Table.S) ->
        if not Table.standalone then
          Table.report_without_accesses ()
      )
      tables;
    lwt_unit

  let initialise () =
    sync_db ();%lwt
    load_tables ();%lwt
    check_dependency_problems ();%lwt
    report_without_accesses ();%lwt
    lwt_unit
end

let initialise = Initialise.initialise
