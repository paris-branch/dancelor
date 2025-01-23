open Dancelor_common
open Model

include PersonLifter.Lift ()

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Get)
