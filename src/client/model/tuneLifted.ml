open Dancelor_common_model

include TuneLifted.Make(Credit)(Dance)

module E = TuneEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
