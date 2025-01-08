open Dancelor_common
include BookLifted

let make_and_save
    ?status
    ~title
    ?date
    ?contents
    ~modified_at
    ~created_at
    ()
  =
  let contents = Option.map (List.map page_to_page_core) contents in
  Madge_client_new.call
    ApiRouter.(route @@ Book MakeAndSave)
    status
    title
    date
    contents
    modified_at
    created_at

let search ?slice ?threshold filter =
  Madge_client_new.call ApiRouter.(route @@ Book Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter

let update
    ?status
    ~slug
    ~title
    ?date
    ?contents
    ~modified_at
    ~created_at
    () (* FIXME: slug as required argument *)
  =
  let contents = Option.map (List.map page_to_page_core) contents in
  Madge_client_new.call
    ApiRouter.(route @@ Book Update)
    status
    title
    date
    contents
    modified_at
    created_at
    slug
