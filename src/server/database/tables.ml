open Nes

module Model = Dancelor_common_model

module Person = Table.Make(struct
  include Model.PersonCore

  let dependencies _ = Lwt.return []
  let standalone = false
end)

module Credit = Table.Make(struct
  include Model.CreditCore

  let dependencies credit =
    let%lwt persons = persons credit in
    List.map (Table.make_slug_and_table (module Person)) persons
    |> Lwt.return

  let standalone = false
end)

module Dance = Table.Make(struct
  include Model.DanceCore

  let dependencies dance =
    match%lwt deviser dance with
    | None -> Lwt.return_nil
    | Some deviser -> Lwt.return [Table.make_slug_and_table (module Credit) deviser]

  let standalone = false
end)

module Tune = Table.Make(struct
  include Model.TuneCore

  let dependencies tune =
    let%lwt dances = dances tune in
    let%lwt author = author tune in
    List.map (Table.make_slug_and_table (module Dance)) dances
    |> (
      match author with
      | None -> Fun.id
      | Some author -> List.cons (Table.make_slug_and_table (module Credit) author)
    )
    |> Lwt.return

  let standalone = false
end)

module Version = Table.Make(struct
  include Model.VersionCore

  let dependencies version =
    let%lwt tune = tune version in
    let%lwt arranger = arranger version in
    []
    |> (
      match arranger with
      | None -> Fun.id
      | Some arranger -> List.cons (Table.make_slug_and_table (module Credit) arranger)
    )
    |> List.cons (Table.make_slug_and_table (module Tune) tune)
    |> Lwt.return

  let standalone = true
end)

module SetModel = struct
  include Model.SetCore

  let dependencies set =
    let%lwt deviser = deviser set in
    let%lwt versions_and_parameters = versions_and_parameters set in
    let versions = List.map fst versions_and_parameters in
    List.map (Table.make_slug_and_table (module Version)) versions
    |> (
      match deviser with
      | None -> Fun.id
      | Some deviser -> List.cons (Table.make_slug_and_table (module Credit) deviser)
    )
    |> Lwt.return

  let standalone = true
end

module Set = Table.Make(SetModel)

module Book = Table.Make(struct
  include Model.BookCore

  let dependencies book =
    let%lwt contents = contents book in
    let%lwt dependencies =
      Lwt_list.map_p
        (
          function
          | PageCore.Version (version, parameters) ->
            Lwt.return
              (
                [Table.make_slug_and_table (module Version) version]
                @
                match Model.VersionParameters.for_dance parameters with
                | None -> []
                | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
          | PageCore.Set (set, parameters) ->
            Lwt.return
              (
                [Table.make_slug_and_table (module Set) set]
                @
                match Model.SetParameters.for_dance parameters with
                | None -> []
                | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
          | PageCore.InlineSet (set, parameters) ->
            let%lwt set_dependencies = SetModel.dependencies set in
            Lwt.return
              (
                set_dependencies
                @
                match Model.SetParameters.for_dance parameters with
                | None -> []
                | Some dance -> [Table.make_slug_and_table (module Dance) dance]
              )
        )
        contents
    in
    Lwt.return (List.flatten dependencies)

  let standalone = true
end)

module Storage = Storage

let tables : (module Table.S) list =
  [
    (module Person);
    (module Credit);
    (module Dance);
    (module Version);
    (module Tune);
    (module Set);
    (module Book);
  ]

module Log = (val Dancelor_server_logs.create "database": Logs.LOG)

module Initialise = struct
  let sync_db () =
    Log.info (fun m -> m "Syncing database changes");
    if (not !Dancelor_server_config.init_only) && !Dancelor_server_config.sync_storage then
      Storage.sync_changes ()
    else
      Lwt.return_unit

  let create_new_db_version () =
    Log.info (fun m -> m "Creating new database version");
    Table.Version.create ()

  let create_tables version =
    Log.info (fun m -> m "Creating tables for this version");
    tables
    |> List.iter
    @@ fun (module Table: Table.S) ->
      Table.create_version ~version

  let load_tables version =
    Log.info (fun m -> m "Loading tables for this version");
    tables
    |> Lwt_list.iter_s
    @@ fun (module Table: Table.S) ->
      Table.load_version ~version

  let check_dependency_problems version =
    Log.info (fun m -> m "Checking for dependency problems");
    let%lwt found_problem =
      Lwt_list.fold_left_s
        (
          fun found_problem (module Table: Table.S) ->
            let%lwt problems = Table.list_dependency_problems ~version in
            (
              problems
              |> List.iter
              @@ function
              | Dancelor_common.Error.DependencyDoesNotExist ((from_key, from_slug), (to_key, to_slug)) ->
                Log.warn (fun m -> m "%s / %s refers to %s / %s that does not exist" from_key from_slug to_key to_slug)
              | DependencyViolatesStatus ((from_key, from_slug), (to_key, to_slug)) ->
                Log.warn (fun m -> m "%s / %s refers to %s / %s but has a higher status" from_key from_slug to_key to_slug)
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
    | Some problem -> Dancelor_common.Error.fail problem

  let report_without_accesses version =
    Log.info (fun m -> m "Checking for unaccessible entries");
    List.iter
      (
        fun (module Table: Table.S) ->
          if not Table.standalone then
            Table.report_without_accesses ~version
      )
      tables;
    Lwt.return ()

  let establish_version version =
    Log.info (fun m -> m "Establishing new version");
    let () =
      tables
      |> List.iter
      @@ fun (module Table: Table.S) ->
        Table.establish_version ~version
    in
    Log.info (fun m -> m "New version is in place")

  let initialise () =
    sync_db ();%lwt
    let version = create_new_db_version () in
    create_tables version;
    load_tables version;%lwt
    check_dependency_problems version;%lwt
    report_without_accesses version;%lwt
    establish_version version;
    Lwt.return_unit
end

let initialise = Initialise.initialise
