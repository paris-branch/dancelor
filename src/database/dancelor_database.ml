module Credit = Credit
module Person = Person
module Program = Program
module Set = Set
module Tune = Tune
module TuneGroup = TuneGroup

module Unsafe = Dancelor_database_unsafe
module Storage = Dancelor_database_storage

module Log = (val Dancelor_common.Log.create "dancelor.database" : Logs.LOG)

let initialise () =
  Unsafe.Person.initialise ();
  Unsafe.Credit.initialise ();
  Unsafe.TuneGroup.initialise ();
  Unsafe.Tune.initialise ();
  Unsafe.Set.initialise ();
  Unsafe.Program.initialise ()

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
