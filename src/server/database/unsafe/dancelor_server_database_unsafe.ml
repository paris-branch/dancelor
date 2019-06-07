module Log = Dancelor_server_logs

module Person = Generic.Make
    (val Log.create "server.database.unsafe.person" : Logs.LOG)
    (Dancelor_common_model.Person)

module Credit = Generic.Make
    (val Log.create "server.database.unsafe.credit" : Logs.LOG)
    (Dancelor_common_model.Credit)

module Source = Generic.Make
    (val Log.create "server.database.unsafe.source" : Logs.LOG)
    (Dancelor_common_model.Source)

module Dance = Generic.Make
    (val Log.create "server.database.unsafe.dance" : Logs.LOG)
    (Dancelor_common_model.Dance)

module Tune = Generic.Make
    (val Log.create "server.database.unsafe.tune" : Logs.LOG)
    (Dancelor_common_model.Tune)

module TuneGroup = Generic.Make
    (val Log.create "server.database.unsafe.tune-group" : Logs.LOG)
    (Dancelor_common_model.TuneGroup)

module Set = Generic.Make
    (val Log.create "server.database.unsafe.set" : Logs.LOG)
    (Dancelor_common_model.Set)

module Program = Generic.Make
    (val Log.create "server.database.unsafe.program" : Logs.LOG)
    (Dancelor_common_model.Program)
