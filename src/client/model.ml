open Nes
open Common

let madge_call_or_option endpoint id =
  flip Lwt.map (Madge_client.call (Endpoints.Api.route @@ endpoint) id) @@ function
    | Ok v -> Some v
    | Error (Madge_client.Http {status = `Not_found; _}) -> None
    | Error e -> raise (Madge_client.Error e)

include ModelBuilder.Build(struct
  let get_book = madge_call_or_option Endpoints.Api.(Book Get)
  let get_dance = madge_call_or_option Endpoints.Api.(Dance Get)
  let get_person = madge_call_or_option Endpoints.Api.(Person Get)
  let get_set = madge_call_or_option Endpoints.Api.(Set Get)
  let get_source = madge_call_or_option Endpoints.Api.(Source Get)
  let get_tune = madge_call_or_option Endpoints.Api.(Tune Get)
  let get_version = madge_call_or_option Endpoints.Api.(Version Get)
end)
