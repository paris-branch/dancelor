open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.credit" : Logs.LOG)

let get (slug : Model.Credit.t Slug.t) = Unsafe.Credit.Static.get slug
let get_all = Unsafe.Credit.Static.get_all

let save = Unsafe.Credit.Static.save
