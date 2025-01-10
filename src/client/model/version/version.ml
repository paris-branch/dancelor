open Nes
open Dancelor_common
include VersionLifted

let make_and_save
    ?status
    ~tune
    ~bars
    ~key
    ~structure
    ?arrangers
    ?remark
    ?disambiguation
    ?broken
    ~content
    ~modified_at
    ~created_at
    ()
  =
  Madge_client_new.call
    ApiRouter.(route @@ Version MakeAndSave)
    status
    tune
    bars
    key
    structure
    arrangers
    remark
    disambiguation
    broken
    content
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Version Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let mark_fixed = Madge_client_new.call ApiRouter.(route @@ Version MarkFixed) % slug
let mark_broken = Madge_client_new.call ApiRouter.(route @@ Version MarkBroken) % slug
