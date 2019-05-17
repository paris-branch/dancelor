include Dancelor_common_model.Tune

let tune_group t =
  let%lwt slug = tune_group t in
  Dancelor_client_api.request
    ~route:(Dancelor_common.Route.TuneGroup slug)
    ~reader:of_yojson
    ()

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Route.Credit slug)
        ~reader:of_yojson
        ()
    in
    Lwt.return_some c
