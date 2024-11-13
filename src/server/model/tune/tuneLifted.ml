open Dancelor_common_model

include TuneLifter.Lift(Person)(Dance)

module E = TuneEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint: E.get @@ fun {a} _ ->
    get (a A.slug)
  )
