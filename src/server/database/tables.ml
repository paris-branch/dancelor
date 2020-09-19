module Model = Dancelor_common_model

module Person = Table.Make (
  struct
    include Model.Person

    let dependencies _ = Lwt.return []
  end)

module Credit = Table.Make (
  struct
    include Model.Credit

  let dependencies credit =
    let%lwt persons = persons credit in
    List.map (Table.make_slug_and_table (module Person)) persons
    |> Lwt.return
  end)

module Source = Table.Make (
  struct
    include Model.Source

    let dependencies _ = Lwt.return []
  end)

module Tune = Table.Make (
  struct
    include Model.Tune

    let dependencies tune =
      match%lwt author tune with
      | None -> Lwt.return_nil
      | Some author ->
        Lwt.return [Table.make_slug_and_table (module Credit) author]
  end)

module Version = Table.Make (
  struct
    include Model.Version

    let dependencies version =
      let%lwt tune = group version in
      let%lwt arranger = arranger version in
      let%lwt sources = sources version in
      List.map (Table.make_slug_and_table (module Source)) sources
      |> (match arranger with
          | None -> Fun.id
          | Some arranger -> List.cons (Table.make_slug_and_table (module Credit) arranger))
      |> List.cons (Table.make_slug_and_table (module Tune) tune)
      |> Lwt.return
  end)

module Dance = Table.Make (
  struct
    include Model.Dance

    let dependencies dance =
      let%lwt deviser = deviser dance in
      let%lwt originals = originals dance in
      List.map (Table.make_slug_and_table (module Version)) originals
      |> (match deviser with
          | None -> Fun.id
          | Some deviser -> List.cons (Table.make_slug_and_table (module Credit) deviser))
      |> Lwt.return
  end)

module Set = Table.Make (
  struct
    include Model.Set

    let dependencies set =
      let%lwt deviser = deviser set in
      let%lwt versions = versions set in
      List.map (Table.make_slug_and_table (module Version)) versions
      |> (match deviser with
          | None -> Fun.id
          | Some deviser -> List.cons (Table.make_slug_and_table (module Credit) deviser))
      |> Lwt.return
  end)

module Program = Table.Make (
  struct
    include Model.Program

    let dependencies program =
      let%lwt sets = sets program in
      List.map (Table.make_slug_and_table (module Set)) sets
      |> Lwt.return
  end)

module Storage = Storage

let tables : (module Table.S) list = [
  (module Person) ;
  (module Credit) ;
  (module Source) ;
  (module Dance) ;
  (module Version) ;
  (module Tune) ;
  (module Set) ;
  (module Program)
]

module Log = (val Dancelor_server_logs.create "database" : Logs.LOG)

module Initialise = struct
  let sync_db () =
    Log.info (fun m -> m "Syncing database changes");
    if (not !Dancelor_server_config.init_only) && !Dancelor_server_config.sync_storage then
      Storage.sync_changes ()
    else
      Lwt.return_unit

  let create_new_db_lyversion () =
    Log.info (fun m -> m "Creating new database lyversion");
    Table.Version.create ()

  let create_tables lyversion =
    Log.info (fun m -> m "Creating tables for this lyversion");
    tables |> List.iter @@ fun (module Table : Table.S) ->
    Table.create_lyversion ~lyversion

  let load_tables lyversion =
    Log.info (fun m -> m "Loading tables for this lyversion");
    tables |> Lwt_list.iter_s @@ fun (module Table : Table.S) ->
    Table.load_lyversion ~lyversion

  let check_dependency_problems lyversion =
    Log.info (fun m -> m "Checking for dependency problems");
    let%lwt found_problem =
      Lwt_list.fold_left_s
        (fun found_problem (module Table : Table.S) ->
           let%lwt problems = Table.list_dependency_problems ~lyversion in
           (
             problems |> List.iter @@ function
             | Dancelor_common.Error.DependencyDoesNotExist ((from_key, from_slug), (to_key, to_slug)) ->
               Log.warn (fun m -> m "%s / %s refers to %s / %s that does not exist" from_key from_slug to_key to_slug)
             | DependencyViolatesStatus ((from_key, from_slug), (to_key, to_slug)) ->
               Log.warn (fun m -> m "%s / %s refers to %s / %s but has a higher status" from_key from_slug to_key to_slug)
             | _ -> ()
           );
           match found_problem, problems with
           | Some found_problem, _ -> Lwt.return_some found_problem
           | _, problem::_ -> Lwt.return_some problem
           | _ -> Lwt.return_none
        )
        None
        tables
    in
    match found_problem with
    | None -> Lwt.return ()
    | Some problem -> Dancelor_common.Error.fail problem

  let establish_lyversion lyversion =
    Log.info (fun m -> m "Establishing new lyversion");
    let () =
      tables |> List.iter @@ fun (module Table : Table.S) ->
      Table.establish_lyversion ~lyversion
    in
    Log.info (fun m -> m "New lyversion is in place")

  let initialise () =
    sync_db (); %lwt
    let lyversion = create_new_db_lyversion () in
    create_tables lyversion;
    load_tables lyversion; %lwt
    check_dependency_problems lyversion; %lwt
    establish_lyversion lyversion;
    Lwt.return_unit
end

let initialise = Initialise.initialise
