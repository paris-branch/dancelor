open Common

let can_create () =
  Madge_client.call_exn Endpoints.Api.(route @@ User CanCreate)

let can_admin () =
  Madge_client.call_exn Endpoints.Api.(route @@ User CanAdmin)
