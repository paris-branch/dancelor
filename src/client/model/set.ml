include Dancelor_common_model.Set

let deviser p =
  match%lwt deviser p with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Route.Credit slug)
        ~reader:of_yojson
        ()
    in
    Lwt.return_some c

let tunes p =
  let%lwt tunes = tunes p in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Route.Tune slug)
         ~reader:of_yojson
         ())
    tunes
