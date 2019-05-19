include Dancelor_common_model.TuneGroup

let author g =
  match%lwt author g with
  | None -> Lwt.return_none
  | Some author ->
    let%lwt author = Dancelor_database.Credit.get author in
    Lwt.return_some author
