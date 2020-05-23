(* open Nes *)

module Person = Person
module Credit = Credit
module Source = Source
module Dance = Dance
module TuneGroup = TuneGroup
module Tune = Tune
module Set = Set
module Program = Program

module Storage = Dancelor_server_database_unsafe.Storage

module Log = (val Dancelor_server_logs.create "database" : Logs.LOG)

let initialise () = Dancelor_server_database_unsafe.State.initialise ()

let check_consistency _database =
  Log.err (fun m -> m "check_consistency not implemented yet."); (* FIXME *)
  Lwt.return ()

let report_without_accesses _database =
  Log.err (fun m -> m "report_without_accesses not implemented yet."); (* FIXME *)
  Lwt.return ()

let establish database = Dancelor_server_database_unsafe.State.establish database
