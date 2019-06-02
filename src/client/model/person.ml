include Dancelor_common_model.Person

(* * *)

let get slug =
  Madge.(call ~endpoint:Endpoint.get @@ fun query ->
         add_arg query Arg.slug slug)
