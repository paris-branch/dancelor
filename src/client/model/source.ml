open Nes
open Common

include ModelBuilder.Source.Build ()

let get = Madge_client.call_exn Endpoints.Api.(route @@ Source Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Source Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Source Update) (* FIXME *)

let search = Madge_client.call_exn Endpoints.Api.(route @@ Source Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
