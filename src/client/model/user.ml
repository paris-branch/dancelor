open Common

include ModelBuilder.User.Build(Person)

let get = Madge_cohttp_lwt_client.call Endpoints.Api.(route @@ User Get)
