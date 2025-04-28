open Nes
open Common

include ModelBuilder.Tune.Build(Dance)(Person)

let get = Madge_client.call_exn Endpoints.Api.(route @@ Tune Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Tune Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Tune Update) (* FIXME *)

let search = Madge_client.call_exn Endpoints.Api.(route @@ Tune Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
