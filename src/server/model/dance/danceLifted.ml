open Dancelor_common_model

include DanceLifter.Lift(Person)

let get = Dancelor_server_database.Dance.get
