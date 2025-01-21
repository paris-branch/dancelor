open Nes
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.TuneEndpoints.t -> a = function
  | Get -> Model.Tune.get
  | Search -> (fun slice threshold filter -> Model.Tune.search ?slice ?threshold filter)
  | Create -> Model.Tune.create
  | Update -> Model.Tune.update
