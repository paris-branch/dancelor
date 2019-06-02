include Dancelor_common_model.Dance

let deviser d =
  match%lwt deviser d with
  | None -> Lwt.return_none
  | Some deviser ->
    let%lwt deviser = Dancelor_server_database.Credit.get deviser in
    Lwt.return_some deviser

(* * *)

let get = Dancelor_server_database.Dance.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun query ->
    get (get_arg query Arg.slug)
  )
