open Dancelor_common
open Model

include SetLifter.Lift(Person)(Dance)(Tune)(Version)

let get = Madge_client_new.call ApiRouter.(route @@ Set Get)
