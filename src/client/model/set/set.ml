open Dancelor_common
include SetLifted

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let delete s = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Delete) (Database.Entry.slug s)

let search ?slice ?threshold filter =
  Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Search) slice threshold filter

let search' ?slice ?threshold filter =
  Lwt.map snd @@ search ?slice ?threshold filter

let count ?threshold filter =
  Lwt.map fst @@ search ?threshold filter
