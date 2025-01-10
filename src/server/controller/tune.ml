open Nes
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.TuneEndpoints.t -> a = function
  | Get -> Model.Tune.get
  | Search -> (fun slice threshold filter -> Model.Tune.search ?slice ?threshold filter)
  | Save -> (fun status name alternative_names kind composers dances remark scddb_id date modified_at created_at -> Model.Tune.save ?status ~name ?alternative_names ~kind ?composers ?dances ?remark ?scddb_id ?date ~modified_at ~created_at ())
