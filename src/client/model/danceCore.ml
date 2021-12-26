open Nes
open Dancelor_common_model
include DanceCore

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

module E = DanceEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
