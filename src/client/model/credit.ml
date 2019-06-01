include Dancelor_common_model.Credit

let persons c =
  let%lwt persons = persons c in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Person slug)
         ~reader:Person.of_yojson
         ())
    persons

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Credit slug)
    ~reader:of_yojson
    ()

let make_and_save ~line ?persons () =
  let%lwt persons =
    match persons with
    | None -> Lwt.return_none
    | Some persons ->
      let%lwt persons = Lwt_list.map_s Person.slug persons in
      Lwt.return_some persons
  in
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.CreditSave
    ~reader:of_yojson
    ~query:(
      ["line", [line]]
      @ (match persons with None -> [] | Some persons -> ["persons", persons])
    )
    ()
