include Dancelor_common_model.Core.SetParameters

let for_dance p =
  let%olwt dance_slug = Lwt.return (for_dance p) in
  let%lwt dance = Dance.get dance_slug in
  Lwt.return_some dance
