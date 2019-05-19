include Dancelor_common_model.Set

let deviser s =
  match%lwt deviser s with
  | None -> Lwt.return_none
  | Some deviser ->
    let%lwt deviser = Dancelor_server_database.Credit.get deviser in
    Lwt.return_some deviser

let tunes s =
  let%lwt tunes = tunes s in
  Lwt_list.map_s Dancelor_server_database.Tune.get tunes

(* * *)

let get = Dancelor_server_database.Set.get

let get_all = Dancelor_server_database.Set.get_all

let save = Dancelor_server_database.Set.save

let delete s =
  let%lwt slug = slug s in
  Dancelor_server_database.Set.delete slug
