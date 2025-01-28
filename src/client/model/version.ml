open Nes
open Common

include ModelBuilder.Version.Build(Person)(Tune)

let get = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Version Get)

let create = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Version Create)
let update = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Version Update)
let save ?slug = match slug with None -> create | Some slug -> update slug

let search = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ Version Search)
let search' = Lwt.map snd % search Slice.everything
let count = Lwt.map fst % search Slice.nothing

module Parameters = struct
  include ModelBuilder.Version.Parameters

  let for_dance p =
    let%olwt dance_slug = Lwt.return (for_dance p) in
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance
end
