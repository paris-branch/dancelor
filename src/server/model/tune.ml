include Dancelor_common_model.Tune

let group t =
  let%lwt group = group t in
  Dancelor_server_database.TuneGroup.get group

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some arranger ->
    let%lwt arranger = Dancelor_server_database.Credit.get arranger in
    Lwt.return_some arranger

let dances t =
  let%lwt dances = dances t in
  Lwt_list.map_s Dancelor_server_database.Dance.get dances

(* * *)

let get = Dancelor_server_database.Tune.get

let all ?filter ?pagination () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let%lwt all =
    match filter with
    | Some filter -> TuneFilter.apply filter all
    | None -> Lwt.return all
  in
  let all = List.sort (fun t1 t2 -> compare (slug t1) (slug t2)) all in (* FIXME: it just works, but we're comparing promises here *)
  let%lwt all =
    match pagination with
    | Some pagination -> Pagination.apply pagination all
    | None -> Lwt.return all
  in
  Lwt.return all
