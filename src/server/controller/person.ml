open Dancelor_server_model

let get person : Person.t Controller.t = fun _ ->
  Dancelor_server_database.Person.get person
