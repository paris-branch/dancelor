open Dancelor_common

type t =
  { slug : Slug.t ;
    name : string }

let make_unsafe ~slug ~name =
  { slug ; name }
