include Dancelor_common_model.Person

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Person slug)
    ~reader:of_yojson
    ()
