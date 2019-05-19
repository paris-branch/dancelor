open Dancelor_server_model

let get credit : Credit.t Controller.t = fun _ ->
  Dancelor_server_database.Credit.get credit
