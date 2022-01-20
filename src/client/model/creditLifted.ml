open Dancelor_common_model

include CreditLifted.Make(Person)

module E = CreditEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
