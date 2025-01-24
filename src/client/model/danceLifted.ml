open Dancelor_common

include Model.Lifter.Dance.Lift(Person)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Dance Get)
