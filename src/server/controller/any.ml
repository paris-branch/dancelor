open Nes
open Common

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun _env endpoint ->
  match endpoint with
  | Search -> Model.Any.search
  | SearchContext -> Model.Any.search_context
