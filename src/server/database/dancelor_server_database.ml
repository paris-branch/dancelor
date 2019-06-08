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

module Log = (val Dancelor_server_logs.create "server.database" : Logs.LOG)

let initialise =
  Person.initialise
  >=>| Credit.initialise
  >=>| Source.initialise
  >=>| Dance.initialise
  >=>| TuneGroup.initialise
  >=>| Tune.initialise
  >=>| Set.initialise
  >=>| Program.initialise

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
