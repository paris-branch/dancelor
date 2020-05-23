open Nes

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

let initialise () =
  try%lwt
    Person.initialise ()
    >>=| Credit.initialise
    >>=| Source.initialise
    >>=| TuneGroup.initialise
    >>=| Tune.initialise
    >>=| Dance.initialise
    >>=| Set.initialise
    >>=| Program.initialise
  with
    Dancelor_common.Error.Exn _ ->
    Log.info (fun m -> m "Initialisation failed; exiting.");
    exit 1

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
