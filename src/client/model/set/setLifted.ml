open Dancelor_common
open Model

include SetLifter.Lift(Person)(Dance)(Tune)(Version)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Get)
