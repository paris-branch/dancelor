open Common
include ModelBuilder.Build(struct
  let get_book = Madge_client.call_exn Endpoints.Api.(route @@ Book Get)
  let get_dance = Madge_client.call_exn Endpoints.Api.(route @@ Dance Get)
  let get_person = Madge_client.call_exn Endpoints.Api.(route @@ Person Get)
  let get_set = Madge_client.call_exn Endpoints.Api.(route @@ Set Get)
  let get_source = Madge_client.call_exn Endpoints.Api.(route @@ Source Get)
  let get_tune = Madge_client.call_exn Endpoints.Api.(route @@ Tune Get)
  let get_version = Madge_client.call_exn Endpoints.Api.(route @@ Version Get)
end)
