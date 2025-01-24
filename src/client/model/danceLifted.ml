open Dancelor_common

include Model.Dance.Lift(Person)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Get)
