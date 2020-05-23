open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.source" : Logs.LOG)

let get (slug : Model.Source.t Slug.t) = Unsafe.Source.Static.get slug
let get_all = Unsafe.Source.Static.get_all

let save = Unsafe.Source.Static.save
