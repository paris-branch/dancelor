open Dancelor_common_model

include BookLifter.Lift(Dance)(Set)(Tune)(Version)

let get = Dancelor_server_database.Book.get
