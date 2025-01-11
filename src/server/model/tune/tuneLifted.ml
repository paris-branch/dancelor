open Dancelor_common
open Model

include TuneLifter.Lift(Person)(Dance)

let get = Dancelor_server_database.Tune.get
