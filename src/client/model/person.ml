include Dancelor_common_model.Person

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
