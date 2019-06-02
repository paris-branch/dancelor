include Dancelor_common_model.Person

(* * *)

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )
