open Nes
open Common
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Tune.t -> a = function
  | Get -> Model.Tune.get
  | Search -> Model.Tune.search
  | Create -> Model.Tune.create
  | Update -> Model.Tune.update
