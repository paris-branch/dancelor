include Dancelor_common_model.TuneGroup

let author t =
  match%lwt author t with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt credit =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Router.Credit slug)
        ~reader:Credit.of_yojson
        ()
    in
    Lwt.return_some credit

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.TuneGroup slug)
    ~reader:of_yojson
    ()
