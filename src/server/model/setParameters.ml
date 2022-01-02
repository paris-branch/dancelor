include Dancelor_common_model.SetParameters

let for_dance p =
  match for_dance p with
  | None -> Lwt.return_none
  | Some dance_slug ->
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance
