open Dancelor_common
open Model

include BookLifter.Lift(Dance)(Set)(Tune)(Version)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Book Get)
