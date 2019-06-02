include Dancelor_common_model.Person

(* * *)

let get = Dancelor_server_database.Person.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )

let make_and_save ~name () =
  Dancelor_server_database.Person.save ~slug_hint:name @@ fun slug ->
  Lwt.return (make ~slug ~name)

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun query ->
    make_and_save
      ~name:   (get_arg     query Arg.name)
      ()
  )
