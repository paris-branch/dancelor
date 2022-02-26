open Dancelor_common_model

include BookLifter.Lift(Dance)(Set)(Version)

module E = BookEndpoints
module A = E.Arguments

let get slug =
  let%lwt book = Dancelor_server_database.Book.get slug in
  (* FIXME: keep propagating instead of failing *)
  Lwt.return (Result.get_ok book)

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
