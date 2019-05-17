include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Route.Set slug)
         ~reader:of_yojson
         ())
    sets
