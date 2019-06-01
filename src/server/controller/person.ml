open Dancelor_server_model
open QueryHelpers

let get person : Person.t Controller.t = fun _ ->
  Person.get person

let save : Person.t Controller.t = fun query ->
  let%lwt name = query_string query "name" in
  Person.make_and_save ~name ()
