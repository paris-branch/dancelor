include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_s Dancelor_database.Person.get persons
