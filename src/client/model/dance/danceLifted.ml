open Dancelor_common
open Model

include DanceLifter.Lift(Person)

let get = Madge_client_new.call ApiRouter.(route @@ Dance Get)
