open Dancelor_common_model

include TuneLifter.Lift(Credit)(Dance)

module E = TuneEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(call ~endpoint: E.get @@
                fun { a } _ ->
                a A.slug slug)
