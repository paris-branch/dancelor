include Dancelor_common_model.TuneFilter

let make ?group_author ?group_kind ?key ?bars () =
  let%lwt group_author =
    match group_author with
    | None -> Lwt.return_none
    | Some group_author ->
      let%lwt group_author = Lwt_list.map_s Credit.slug group_author in
      Lwt.return_some group_author
  in
  make ?group_author ?group_kind ?key ?bars ()

let group_author f =
  let%lwt author = group_author f in
  Lwt_list.map_s Credit.get author
