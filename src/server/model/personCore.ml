include Dancelor_common_model.PersonCore

module E = Dancelor_common_model.Person_endpoints
module A = E.Arguments

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
