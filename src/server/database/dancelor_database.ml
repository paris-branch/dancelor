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
  let (>>=) = Lwt.bind in
  Unsafe.Person.initialise () >>= fun () ->
  Unsafe.Credit.initialise () >>= fun () ->
  Unsafe.TuneGroup.initialise () >>= fun () ->
  Unsafe.Tune.initialise () >>= fun () ->
  Unsafe.Set.initialise () >>= fun () ->
  Unsafe.Program.initialise ()

let report_without_accesses () =
  Log.err (fun m -> m "report_without_accesses not implemented yet.") (* FIXME *)
