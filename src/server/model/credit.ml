include Dancelor_common_model.Credit

let get = Dancelor_server_database.Credit.get

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_s Dancelor_server_database.Person.get persons
