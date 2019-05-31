include Dancelor_common_model.Dance

let deviser c =
  match%lwt deviser c with
  | None -> Lwt.return_none
  | Some deviser ->
    let%lwt deviser =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Router.Credit deviser)
        ~reader:Credit.of_yojson
        ()
    in
    Lwt.return_some deviser

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Dance slug)
    ~reader:of_yojson
    ()
