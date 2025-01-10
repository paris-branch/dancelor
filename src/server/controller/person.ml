open Nes
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.PersonEndpoints.t -> a = function
  | Get -> Model.Person.get
  | Search -> (fun slice threshold filter -> Model.Person.search ?slice ?threshold filter)
  | Save -> (fun status name scddb_id modified_at created_at -> Model.Person.save ?status ~name ?scddb_id ~modified_at ~created_at ())
