module E = Dancelor_common_model.Source_endpoints
module A = E.Arguments

include Dancelor_common_model.Source

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let make_and_save ?status ~name () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name
  )

(* let search ?pagination ?threshold string = *)
(*   Madge_client.( *)
(*     call ~endpoint:E.search @@ fun {a} {o} -> *)
(*     o A.pagination pagination; *)
(*     o A.threshold threshold; *)
(*     a A.string string *)
(*   ) *)
