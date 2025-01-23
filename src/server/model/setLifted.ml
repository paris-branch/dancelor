open Dancelor_common_model

include SetLifter.Lift(Person)(Dance)(Tune)(Version)

let get = Dancelor_server_database.Set.get
