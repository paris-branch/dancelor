open Dancelor_common

type t =
  { slug : t Slug.t ;
    name : string ;
    kind : Kind.dance ;
    credit : Credit.t Slug.t }
