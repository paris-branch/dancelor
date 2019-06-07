module Person = Person
module Credit = Credit
module Source = Source
module Dance = Dance
module TuneGroup = TuneGroup
module Tune = Tune
module Set = Set
module Program = Program

module Unsafe = Dancelor_server_database_unsafe
module Storage = Unsafe.Storage

module Log = (val Dancelor_server_logs.create "server.database" : Logs.LOG)

let initialise () =
  let%lwt () = Unsafe.Person.initialise () in
  let%lwt () = Unsafe.Credit.initialise () in
  let%lwt () = Unsafe.Source.initialise () in
  let%lwt () = Unsafe.Dance.initialise () in
  let%lwt () = Unsafe.TuneGroup.initialise () in
  let%lwt () = Unsafe.Tune.initialise () in
  let%lwt () = Unsafe.Set.initialise () in
  let%lwt () = Unsafe.Program.initialise () in
  Lwt.return_unit

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
