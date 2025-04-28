open Nes
open Common

include ModelBuilder.Set.Build(Dance)(Person)(Tune)(Version)

let get = Madge_client.call_exn Endpoints.Api.(route @@ Set Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Set Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Set Update) (* FIXME *)

let delete = Madge_client.call_exn Endpoints.Api.(route @@ Set Delete) % Entry.slug

let search = Madge_client.call_exn Endpoints.Api.(route @@ Set Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
