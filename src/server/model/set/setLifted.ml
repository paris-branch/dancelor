open Dancelor_common_model

include SetLifter.Lift(Credit)(Dance)(Tune)(Version)

module E = SetEndpoints
module A = E.Arguments

let get slug =
  let%lwt set = Dancelor_server_database.Set.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok set)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
