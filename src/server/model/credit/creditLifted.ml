open Dancelor_common_model

include CreditLifter.Lift(Person)

module E = CreditEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(register ~endpoint: E.get @@
                fun { a } _ ->
                get (a A.slug))
