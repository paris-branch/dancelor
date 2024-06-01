open Nes
open Dancelor_common
include VersionLifted

let save
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arrangers
    ?remark
    ?disambiguation
    ~content
    ~modified_at
    ~created_at
    ()
  =
  Madge_cohttp_lwt_client.call
    ApiRouter.(route @@ Version Save)
    status
    tune
    bars
    key
    structure
    arrangers
    remark
    disambiguation
    content
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Version Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
