open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.dance" : Logs.LOG)

let get (slug : Model.Dance.t Slug.t) = Unsafe.Dance.Static.get slug
let get_all = Unsafe.Dance.Static.get_all
