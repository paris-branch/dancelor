open Dancelor_common

type t =
  { slug : Slug.t ;
    credit : string ;
    persons : Person.t list }

let make_unsafe ~slug ~credit ~persons =
  { slug ; credit ; persons }
