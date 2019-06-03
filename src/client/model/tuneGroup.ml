include Dancelor_common_model.TuneGroup

let author t =
  match%lwt author t with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt credit = Credit.get slug in
    Lwt.return_some credit

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )
