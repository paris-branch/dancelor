open Nes
open Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe.Tune

let get (slug : Tune.t Slug.t) = Unsafe.get slug
let get_all () = Unsafe.get_all ()
