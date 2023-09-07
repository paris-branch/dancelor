open Dancelor_common_model

include SetLifter.Lift(Credit)(Dance)(Tune)(Version)

module E = SetEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Set.get

let () =
  Madge_server.(register ~endpoint: E.get @@
                fun { a } _ ->
                get (a A.slug))
