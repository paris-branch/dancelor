include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_s Dancelor_server_database.Set.get sets
