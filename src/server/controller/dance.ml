open Dancelor_server_model

let get dance : Dance.t Controller.t = fun _ ->
    Dance.get dance
