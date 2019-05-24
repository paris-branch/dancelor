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

let get_all ?search ?threshold ?hard_limit () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.TuneAll
    ~reader:Dancelor_common.(Unserializer.list (Dancelor_common_model.Score.of_yojson of_yojson))
    ~query:(
      (match search with None -> [] | Some search -> ["search", search])
      @ (match threshold with None -> [] | Some threshold -> ["threshold", [string_of_float threshold]])
      @ (match hard_limit with None -> [] | Some hard_limit -> ["hard_limit", [string_of_int hard_limit]])
    )
    ()
