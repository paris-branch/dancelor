open Dancelor_common
open Model

include TuneLifter.Lift(Person)(Dance)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Tune Get)
