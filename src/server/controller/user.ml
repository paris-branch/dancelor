open Nes
open Common

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.User.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> Model.User.get
