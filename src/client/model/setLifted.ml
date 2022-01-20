open Dancelor_common_model

include SetLifted.Make(Credit)(Dance)(Tune)(Version)

module E = SetEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
