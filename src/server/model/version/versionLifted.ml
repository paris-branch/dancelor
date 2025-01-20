open Dancelor_common_model

include VersionLifter.Lift(Person)(Tune)

let get = Dancelor_server_database.Version.get
