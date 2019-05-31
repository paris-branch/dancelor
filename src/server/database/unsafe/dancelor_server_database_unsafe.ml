module Log = Dancelor_server_logs

module Person = Generic.Make
    (val Log.create "server.database.unsafe.person" : Logs.LOG)
    (struct
      include Dancelor_common_model.Person

      let prefix = "person"
      let separated_files = []
    end)

module Credit = Generic.Make
    (val Log.create "server.database.unsafe.credit" : Logs.LOG)
    (struct
      include Dancelor_common_model.Credit

      let prefix = "credit"
      let separated_files = []
    end)

module Dance = Generic.Make
    (val Log.create "server.database.unsafe.dance" : Logs.LOG)
    (struct
      include Dancelor_common_model.Dance

      let prefix = "dance"
      let separated_files = []
    end)

module Tune = Generic.Make
    (val Log.create "server.database.unsafe.tune" : Logs.LOG)
    (struct
      include Dancelor_common_model.Tune

      let prefix = "tune"
      let separated_files = []
    end)

module TuneGroup = Generic.Make
    (val Log.create "server.database.unsafe.tune-group" : Logs.LOG)
    (struct
      include Dancelor_common_model.TuneGroup

      let prefix = "tune-group"
      let separated_files = []
    end)

module Set = Generic.Make
    (val Log.create "server.database.unsafe.set" : Logs.LOG)
    (struct
      include Dancelor_common_model.Set

      let prefix = "set"
      let separated_files = []
    end)

module Program = Generic.Make
    (val Log.create "server.database.unsafe.program" : Logs.LOG)
    (struct
      include Dancelor_common_model.Program

      let prefix = "program"
      let separated_files = []
    end)
