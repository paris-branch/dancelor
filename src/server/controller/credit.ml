open Dancelor_server_model

let get credit : Credit.t Controller.t = fun _ ->
  Credit.get credit
