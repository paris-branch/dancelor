open Dancelor_common
open Dancelor_model
module Unsafe = Dancelor_database_unsafe.Person

let get (slug : Person.t Slug.t) = Unsafe.get slug
let get_opt (slug : Person.t Slug.t) = Unsafe.get_opt slug
let get_all () = Unsafe.get_all ()
