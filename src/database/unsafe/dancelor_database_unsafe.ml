module Log = Dancelor_common.Log

module Person = Generic.Make
    (val Log.create "dancelor.database.unsafe.person" : Logs.LOG)
    (struct
      include Dancelor_model.Person

      let prefix = "person"
      let separated_files = []
    end)

module Credit = Generic.Make
    (val Log.create "dancelor.database.unsafe.credit" : Logs.LOG)
    (struct
      include Dancelor_model.Credit

      let prefix = "credit"
      let separated_files = []
    end)

module Tune = Generic.Make
    (val Log.create "dancelor.database.unsafe.tune" : Logs.LOG)
    (struct
      include Dancelor_model.Tune

      let prefix = "tune"
      let separated_files = ["content.ly"]
    end)

module TuneGroup = Generic.Make
    (val Log.create "dancelor.database.unsafe.tune-group" : Logs.LOG)
    (struct
      include Dancelor_model.TuneGroup

      let prefix = "tune-group"
      let separated_files = []
    end)

module Set = Generic.Make
    (val Log.create "dancelor.database.unsafe.set" : Logs.LOG)
    (struct
      include Dancelor_model.Set

      let prefix = "set"
      let separated_files = []
    end)

module Program = Generic.Make
    (val Log.create "dancelor.database.unsafe.program" : Logs.LOG)
    (struct
      include Dancelor_model.Program

      let prefix = "program"
      let separated_files = []
    end)
