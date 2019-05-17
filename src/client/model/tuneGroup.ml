include Dancelor_common_model.TuneGroup

let author t =
  let%lwt slug = author t in
  Dancelor_client_api.request
    ~route:(Dancelor_common.Route.Credit slug)
    ~reader:of_yojson
    ()
