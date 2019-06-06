open Nes
include Dancelor_common_model.Dance

let deviser = deviser >=>|? (Credit.get >=>|| Lwt.return_some)

(* * *)

let get = Dancelor_server_database.Dance.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )
