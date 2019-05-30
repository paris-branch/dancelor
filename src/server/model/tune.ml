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

let apply_filter filter all =
  let%lwt f_group_author = TuneFilter.group_author filter in
  let%lwt f_group_kind = TuneFilter.group_kind filter in
  let%lwt f_key = TuneFilter.key filter in
  let%lwt f_bars = TuneFilter.bars filter in
  Lwt_list.filter_s
    (fun tune ->
       let%lwt group = group tune in
       let%lwt group_author = TuneGroup.author group in
       let%lwt group_kind = TuneGroup.kind group in
       let%lwt key = key tune in
       let%lwt bars = bars tune in
       Lwt.return
         (
           (f_group_author = [] || (match group_author with None -> true | Some group_author -> List.mem group_author f_group_author))
           && (f_group_kind = [] || List.mem group_kind f_group_kind)
           && (f_key = [] || List.mem key f_key)
           && (f_bars = [] || List.mem bars f_bars)
         ))
    all


let all ?filter ?pagination () =
  let%lwt all = Dancelor_server_database.Tune.get_all () in
  let%lwt all =
    match filter with
    | Some filter -> apply_filter filter all
    | None -> Lwt.return all
  in
  let all = List.sort (fun t1 t2 -> compare (slug t1) (slug t2)) all in (* FIXME: it just works, but we're comparing promises here *)
  let%lwt all =
    match pagination with
    | Some pagination -> Pagination.apply pagination all
    | None -> Lwt.return all
  in
  Lwt.return all
