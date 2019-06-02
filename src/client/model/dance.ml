include Dancelor_common_model.Dance

let deviser c =
  match%lwt deviser c with
  | None -> Lwt.return_none
  | Some deviser ->
    let%lwt deviser = Credit.get deviser in
    Lwt.return_some deviser

(* * *)

let get slug =
  Madge.(call ~endpoint:Endpoint.get @@ fun query ->
         add_arg query Arg.slug slug)
