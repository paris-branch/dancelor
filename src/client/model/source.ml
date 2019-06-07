include Dancelor_common_model.Source

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let make_and_save ~name () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun {a} _ ->
    a Arg.name name
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
