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

let make_and_save ~line ?persons () =
  let%lwt persons =
    match persons with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.slug persons in
      Lwt.return_some persons
  in
  Dancelor_server_database.Credit.save ~slug_hint:line @@ fun slug ->
  Lwt.return (make ~slug ~line ?persons ())

let () =
  Madge_server.(
    register ~endpoint:Endpoint.make_and_save @@ fun query ->
    make_and_save
      ~line:   (get_arg     query Arg.line)
      ?persons:(get_opt_arg query Arg.persons)
      ()
  )
