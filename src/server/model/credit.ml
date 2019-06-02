include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_s Dancelor_server_database.Person.get persons

(* * *)

let get = Dancelor_server_database.Credit.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )
