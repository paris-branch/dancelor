open Nes
module Model = Dancelor_server_model

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common.Model.Endpoints.Any.t -> a = function
  | Search -> Model.Any.search
  | SearchContext -> Model.Any.search_context
