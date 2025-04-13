open Nes
open Common

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> Model.Person.get
  | Search -> Model.Person.search
  | Create -> Model.Person.create
  | Update -> Model.Person.update
