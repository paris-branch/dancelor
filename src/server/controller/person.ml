open Nes
open Common
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Person.t -> a = function
  | Get -> Model.Person.get
  | Search -> Model.Person.search
  | Create -> Model.Person.create
  | Update -> Model.Person.update
