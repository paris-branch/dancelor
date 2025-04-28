open Nes
open Common

include ModelBuilder.Person.Build ()

let get = Madge_client.call_exn Endpoints.Api.(route @@ Person Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Person Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Person Update) (* FIXME *)

let search = Madge_client.call_exn Endpoints.Api.(route @@ Person Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
