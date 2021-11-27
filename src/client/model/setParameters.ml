include Dancelor_common_model.SetParameters

let show_dance p =
  match show_dance p with
  | None -> Lwt.return_none
  | Some dance_slug ->
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance
