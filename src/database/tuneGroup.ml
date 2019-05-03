open Nes
open Dancelor_common_model
module Unsafe = Dancelor_database_unsafe.TuneGroup

let get (slug : TuneGroup.t Slug.t) = Unsafe.get slug
let get_opt (slug : TuneGroup.t Slug.t) = Unsafe.get_opt slug
let get_all () = Unsafe.get_all ()
