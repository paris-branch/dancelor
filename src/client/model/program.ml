include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Set slug)
         ~reader:Set.of_yojson
         ())
    sets
