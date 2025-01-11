open Dancelor_common
open Model

include DanceLifter.Lift(Person)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Get)
