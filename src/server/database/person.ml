open Nes
open Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe.Person

let get (slug : Person.t Slug.t) = Unsafe.get slug
let get_all () = Unsafe.get_all ()
