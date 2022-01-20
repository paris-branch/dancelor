open Nes
include Dancelor_common_model.DanceCore

let deviser = deviser >=>?| (CreditLifted.get >=>| Lwt.return_some)

module E = Dancelor_common_model.DanceEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Dance.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
