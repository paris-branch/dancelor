include Dancelor_common_model.Person

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Person slug)
    ~reader:of_yojson
    ()

let make_and_save ~name () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.PersonSave
    ~reader:of_yojson
    ~query:(
      ["name", [name]]
    )
    ()
