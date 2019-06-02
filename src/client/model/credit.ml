include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_p Person.get persons

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun query ->
    add_arg query Arg.slug slug
  )

let make_and_save ~line ?persons () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun query ->
    add_arg     query Arg.line line;
    add_opt_arg query Arg.persons persons
  )
