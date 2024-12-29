open Dancelor_common_model

include DanceLifter.Lift(Person)

module E = DanceEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Dance.get

let () =
  Madge_server.(
    register ~endpoint: E.get @@ fun {a} _ ->
    get (a A.slug)
  )
