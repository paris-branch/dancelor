include Dancelor_common_model.VersionParameters

let for_dance p =
  let%optlwt dance_slug = Lwt.return (for_dance p) in
  let%lwt dance = Dance.get dance_slug in
  Lwt.return_some dance
