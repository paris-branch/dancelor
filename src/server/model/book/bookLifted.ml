open Dancelor_common_model

include BookLifter.Lift(Dance)(Set)(Tune)(Version)

module E = BookEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(register ~endpoint: E.get @@
                fun { a } _ ->
                get (a A.slug))
