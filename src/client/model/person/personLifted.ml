open Dancelor_common_model
include PersonCore

module E = PersonEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(call ~endpoint: E.get
  @@ fun { a } _ ->
    a A.slug slug)
