open Dancelor_common
open Model

include TuneLifter.Lift(Person)(Dance)

let get = Madge_client_new.call ApiRouter.(route @@ Tune Get)
