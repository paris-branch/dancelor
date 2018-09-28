open Dancelor_common

type t =
  { slug : Slug.t ;
    name : string ;
    kind : Kind.dance ;
    tunes : Tune.t list }
