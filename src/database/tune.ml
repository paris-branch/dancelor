open Dancelor_common
open Dancelor_model
module Unsafe = Dancelor_database_unsafe.Tune

let get (slug : Tune.t Slug.t) = Unsafe.get slug
let get_opt (slug : Tune.t Slug.t) = Unsafe.get_opt slug
let get_all () = Unsafe.get_all ()
