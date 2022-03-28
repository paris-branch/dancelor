include Dancelor_common_model.PersonCore

module E = Dancelor_common_model.PersonEndpoints
module A = E.Arguments

let get slug =
  let%lwt person = Dancelor_server_database.Person.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok person)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
