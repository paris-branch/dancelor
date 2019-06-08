open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.Source.t Slug.t) = Unsafe.Source.get slug
let get_all = Unsafe.Source.get_all
let save = Unsafe.Source.save
let initialise = Unsafe.Source.initialise
