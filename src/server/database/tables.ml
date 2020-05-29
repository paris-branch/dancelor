module Model = Dancelor_common_model

module Person = Table.Make (
  struct
    include Model.Person

    let dependencies _ = Lwt.return []
  end)

module Credit = Table.Make (
  struct
    include Model.Credit

    let dependencies _ = Lwt.return []
  end)

module Source = Table.Make (
  struct
    include Model.Source

    let dependencies _ = Lwt.return []
  end)

module Dance = Table.Make (
  struct
    include Model.Dance

    let dependencies _ = Lwt.return []
  end)

module Tune = Table.Make (
  struct
    include Model.Tune

    let dependencies _ = Lwt.return []
  end)

module TuneGroup = Table.Make (
  struct
    include Model.TuneGroup

    let dependencies _ = Lwt.return []
  end)

module Set = Table.Make (
  struct
    include Model.Set

    let dependencies _ = Lwt.return []
  end)

module Program = Table.Make (
  struct
    include Model.Program

    let dependencies _ = Lwt.return []
  end)

module Storage = Storage

let tables : (module Table.S) list = [
  (module Person) ;
  (module Credit) ;
  (module Source) ;
  (module Dance) ;
  (module Tune) ;
  (module TuneGroup) ;
  (module Set) ;
  (module Program)
]

let initialise () =
  let version = Table.Version.create () in
  let () =
    tables |> List.iter @@ fun (module Table : Table.S) ->
    Table.create_version ~version
  in
  let%lwt () =
    tables |> Lwt_list.iter_s @@ fun (module Table : Table.S) ->
    Table.load_version ~version
  in
  let () =
    tables |> List.iter @@ fun (module Table : Table.S) ->
    Table.establish_version ~version
  in
  Lwt.return ()
