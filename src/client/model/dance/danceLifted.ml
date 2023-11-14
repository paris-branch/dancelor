open Dancelor_common_model

include DanceLifter.Lift(Person)

module E = DanceEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )
