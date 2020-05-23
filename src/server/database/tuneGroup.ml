open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.tune-group" : Logs.LOG)

let get (slug : Model.TuneGroup.t Slug.t) = Unsafe.TuneGroup.get slug
let get_all = Unsafe.TuneGroup.get_all
