open Nes
open Common

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> Model.Tune.get
  | Search -> Model.Tune.search
  | Create -> Model.Tune.create
  | Update -> Model.Tune.update
