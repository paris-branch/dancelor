open Dancelor_common_model

include CreditLifter.Lift(Person)

module E = CreditEndpoints
module A = E.Arguments

let get slug =
  let%lwt credit = Dancelor_server_database.Credit.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok credit)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
