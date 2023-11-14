open Dancelor_common_model

include PersonLifter.Lift()

module E = PersonEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
