open Nes
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.AnyEndpoints.t -> a = function
  | Search -> (fun slice threshold filter -> Model.Any.search ?slice ?threshold filter)
  | SearchContext -> (fun threshold filter element -> Model.Any.search_context ?threshold filter element)
