open Dancelor_common
open Model

include PersonLifter.Lift ()

let get = Madge_client_new.call ApiRouter.(route @@ Person Get)
