open Nes
open Common

let madge_call_or_option endpoint id =
  Lwt.flip_map (Madge_client.call (Endpoints.Api.route @@ endpoint) id) @@ function
    | Ok v -> Some v
    | Error (Madge_client.Http {status = `Not_found; _}) -> None
    | Error e -> raise (Madge_client.Error e)

include Model_builder.Build(struct
  let get_user = madge_call_or_option (User Get)
  let get_book = madge_call_or_option (Book Get)
  let get_dance = madge_call_or_option (Dance Get)
  let get_person = madge_call_or_option (Person Get)
  let get_set = madge_call_or_option (Set Get)
  let get_source = madge_call_or_option (Source Get)
  let get_tune = madge_call_or_option (Tune Get)
  let get_version = madge_call_or_option (Version Get)
end)
