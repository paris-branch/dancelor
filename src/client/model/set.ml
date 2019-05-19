include Dancelor_common_model.Set

let deviser s =
  match%lwt deviser s with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Router.Credit slug)
        ~reader:Credit.of_yojson
        ()
    in
    Lwt.return_some c

let tunes s =
  let%lwt tunes = tunes s in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Tune slug)
         ~reader:Tune.of_yojson
         ())
    tunes

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Set slug)
    ~reader:of_yojson
    ()

let get_all () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.SetAll
    ~reader:(Dancelor_common.Unserializer.list of_yojson)
    ()

let save ?slug ~name ~kind ?status ~tunes () =
  let%lwt tunes = Lwt_list.map_s Tune.slug tunes in
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.SetSave
    ~reader:of_yojson
    ~query:(
      (match slug with None -> [] | Some slug -> ["slug", [slug]])
      @ ["name", [name]]
      @ ["kind", [Kind.dance_to_string kind]]
      @ (match status with None -> [] | Some status -> ["status", [Status.to_string status]])
      @ ["tunes", tunes]
    )
    ()

let delete s =
  let%lwt slug = slug s in (* FIXME: SetDelete could maybe take a set directly? *)
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.SetDelete slug)
    ~reader:Dancelor_common.Unserializer.unit
    ()
