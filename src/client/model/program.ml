include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Set slug)
         ~reader:Set.of_yojson
         ())
    sets

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Program slug)
    ~reader:of_yojson
    ()

let get_all () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.ProgramAll
    ~reader:(Dancelor_common.Unserializer.list of_yojson)
    ()
