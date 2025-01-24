open Nes
open Dancelor_common

include Model.Set.Lift(Person)(Dance)(Tune)(Version)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Get)

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let delete s = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Delete) (Database.Entry.slug s)

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Set Search)
let search' = Lwt.map snd % search Model.Slice.everything
let count = Lwt.map fst % search Model.Slice.nothing
