open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.tune" : Logs.LOG)

let get (slug : Model.Tune.t Slug.t) = Unsafe.Tune.Static.get slug
let get_all = Unsafe.Tune.Static.get_all

let read_content tune =
  Unsafe.Tune.read_separated_file tune "content.ly"

let write_content tune content =
  Unsafe.Tune.write_separated_file tune "content.ly" content
