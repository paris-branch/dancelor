include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Route.Person slug)
         ~reader:of_yojson
         ())
    persons
