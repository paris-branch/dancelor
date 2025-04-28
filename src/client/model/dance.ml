open Nes
open Common

include ModelBuilder.Dance.Build(Person)

let get = Madge_client.call_exn Endpoints.Api.(route @@ Dance Get)

let create = Madge_client.call_exn Endpoints.Api.(route @@ Dance Create) (* FIXME *)
let update = Madge_client.call_exn Endpoints.Api.(route @@ Dance Update) (* FIXME *)

let search = Madge_client.call_exn Endpoints.Api.(route @@ Dance Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing
