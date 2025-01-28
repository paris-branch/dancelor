open Nes
open Dancelor_common

include Dancelor_common.Model.Person.Lift()

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Get)

let create = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Create)
let update = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Person Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
