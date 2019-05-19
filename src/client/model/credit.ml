include Dancelor_common_model.Credit

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Credit slug)
    ~reader:of_yojson
    ()

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Person slug)
         ~reader:Person.of_yojson
         ())
    persons
