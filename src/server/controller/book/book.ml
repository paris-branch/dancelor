open NesUnix
open Common

module Ly = Ly
module Pdf = Pdf

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> Model.Book.get
  | Search -> Model.Book.search
  | Create -> Model.Book.create
  | Update -> Model.Book.update
  | Pdf -> Pdf.get
