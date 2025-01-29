open Nes
open Common

let dispatch : type a r. (a, r Lwt.t, r) Endpoints.Any.t -> a = function
  | Search -> Model.Any.search
  | SearchContext -> Model.Any.search_context
