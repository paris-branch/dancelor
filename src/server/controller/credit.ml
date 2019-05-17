open Dancelor_server_model

let get credit : Credit.t Controller.t = fun _ ->
  Dancelor_database.Credit.get credit
