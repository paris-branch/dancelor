open Nes
include Dancelor_common_model.Tune

let author = author >=>?| (Credit.get >=>| Lwt.return_some)

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
