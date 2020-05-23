open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.program" : Logs.LOG)

let get (slug : Model.Program.t Slug.t) = Unsafe.Program.Static.get slug
let get_all = Unsafe.Program.Static.get_all
