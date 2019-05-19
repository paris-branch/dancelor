open Dancelor_server_model

let get person : Person.t Controller.t = fun _ ->
  Person.get person
