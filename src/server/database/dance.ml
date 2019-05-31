open Nes
open Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe.Dance

let get (slug : Dance.t Slug.t) = Unsafe.get slug
let get_all () = Unsafe.get_all ()
