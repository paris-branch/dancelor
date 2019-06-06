open Nes
include Dancelor_common_model.Dance

let deviser = deviser >=>|? (Credit.get >=>|| Lwt.return_some)

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )
