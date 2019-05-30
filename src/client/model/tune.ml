include Dancelor_common_model.Tune

let group t =
  let%lwt slug = group t in
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.TuneGroup slug)
    ~reader:TuneGroup.of_yojson
    ()

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Router.Credit slug)
        ~reader:Credit.of_yojson
        ()
    in
    Lwt.return_some c

let dances t =
  let%lwt dances = dances t in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Dance slug)
         ~reader:Dance.of_yojson
         ())
    dances

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Tune slug)
    ~reader:of_yojson
    ()

let all ?filter ?pagination () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.TuneAll
    ~reader:Dancelor_common.(Unserializer.list of_yojson)
    ~query:(
      (match filter with Some filter -> ["filter", [NesJson.to_string (TuneFilter.to_yojson filter)]] | _ -> [])
      @ (match pagination with Some pagination -> ["pagination", [NesJson.to_string (Pagination.to_yojson pagination)]] | _ -> [])
    )
    ()
