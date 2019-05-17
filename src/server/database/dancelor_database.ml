module Credit = Credit
module Person = Person
module Program = Program
module Set = Set
module Tune = Tune
module TuneGroup = TuneGroup

module Unsafe = Dancelor_database_unsafe
module Storage = Dancelor_database_storage

module Log = (val Dancelor_server_logs.create "database" : Logs.LOG)

let initialise () =
  let%lwt () = Unsafe.Person.initialise () in
  let%lwt () = Unsafe.Credit.initialise () in
  let%lwt () = Unsafe.TuneGroup.initialise () in
  let%lwt () = Unsafe.Tune.initialise () in
  let%lwt () = Unsafe.Set.initialise () in
  Unsafe.Program.initialise ()

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
