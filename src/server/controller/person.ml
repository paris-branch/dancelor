open Dancelor_server_model
let (>>=) = Lwt.bind

let get person : Person.t Controller.t = fun _ ->
  Dancelor_database.Person.get person
