include Dancelor_common_model.Tune

let group t =
  let%lwt group = group t in
  Dancelor_database.TuneGroup.get group

let arranger t =
  match%lwt arranger t with
  | None -> Lwt.return_none
  | Some arranger ->
    let%lwt arranger = Dancelor_database.Credit.get arranger in
    Lwt.return_some arranger
