open NesUnix
open Common

module Ly = Ly
module Svg = Svg
module Ogg = Ogg
module Pdf = Pdf

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> Model.Version.get
  | Search -> Model.Version.search
  | Create -> Model.Version.create
  | Update -> Model.Version.update
  | Ly -> Ly.get
  | Svg -> Svg.get
  | Ogg -> Ogg.get
  | Pdf -> Pdf.get
  | PreviewSvg -> Svg.preview
  | PreviewOgg -> Ogg.preview
