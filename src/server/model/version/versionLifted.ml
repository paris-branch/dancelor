open Dancelor_common_model

include VersionLifter.Lift(Credit)(Tune)

let content version =
  let%lwt content = Dancelor_server_database.Version.read_content version in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok content)

module E = Dancelor_common_model.VersionEndpoints
module A = E.Arguments

let get slug =
  let%lwt version = Dancelor_server_database.Version.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok version)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
