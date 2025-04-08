open Nes
open Common

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Source.t -> a = function
  | Get -> Model.Source.get
  | Search -> Model.Source.search
  | Create -> Model.Source.create
  | Update -> Model.Source.update
