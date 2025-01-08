open Dancelor_common
open Model

include PersonLifter.Lift ()

let get = Dancelor_server_database.Person.get
