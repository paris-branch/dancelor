open Dancelor_common_model

include TuneLifter.Lift(Credit)(Dance)

module E = TuneEndpoints
module A = E.Arguments

let get slug =
  let%lwt tune = Dancelor_server_database.Tune.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok tune)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
