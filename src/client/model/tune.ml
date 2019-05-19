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

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Tune slug)
    ~reader:of_yojson
    ()

let get_all ?kind ?keys ?mode ?name ?author ?threshold ?hard_limit () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.TuneAll
    ~reader:Dancelor_common.(Unserializer.list (Dancelor_common_model.Score.of_yojson of_yojson))
    ~query:(
      (match kind with None -> [] | Some kind -> ["kind", [Kind.base_to_string kind]])
      @ (match keys with None -> [] | Some keys -> ["keys", List.map Music.key_to_string keys])
      @ (match mode with None -> [] | Some mode -> ["mode", [Music.mode_to_string mode]])
      @ (match name with None -> [] | Some name -> ["name", [name]])
      @ (match author with None -> [] | Some author -> ["author", [author]])
      @ (match threshold with None -> [] | Some threshold -> ["threshold", [string_of_float threshold]])
      @ (match hard_limit with None -> [] | Some hard_limit -> ["hard_limit", [string_of_int hard_limit]])
    )
    ()
