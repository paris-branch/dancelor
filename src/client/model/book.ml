open Nes
open Common

include ModelBuilder.Book.Build(Dance)(Set)(Tune)(Version)

let get = Madge_client.call_exn Endpoints.Api.(route @@ Book Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Book Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Book Update) (* FIXME *)

let search = Madge_client.call_exn Endpoints.Api.(route @@ Book Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
