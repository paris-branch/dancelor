open Dancelor_common

type t =
  { slug : Slug.t ;
    name : string }

let name p = p.name
  
let make_unsafe ~slug ~name =
  { slug ; name }
