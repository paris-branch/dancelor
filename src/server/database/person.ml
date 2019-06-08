open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.Person.t Slug.t) = Unsafe.Person.get slug
let get_all = Unsafe.Person.get_all
let save = Unsafe.Person.save
let initialise = Unsafe.Person.initialise
