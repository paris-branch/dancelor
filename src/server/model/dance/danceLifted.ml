open Dancelor_common_model

include DanceLifter.Lift(Credit)

module E = DanceEndpoints
module A = E.Arguments

let get slug =
  let%lwt dance = Dancelor_server_database.Dance.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok dance)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
