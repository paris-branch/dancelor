open Nes
include Dancelor_common_model.SetParameters

let for_dance p =
  let%olwt dance_slug = Lwt.return (for_dance p) in
  let%lwt dance = Dance.get dance_slug in
  Lwt.return_some dance

let make
    ?instruments
    ?forced_pages
    ?show_deviser
    ?show_order
    ?display_name
    ?for_dance
    ?every_version
    ()
  =
  let%lwt for_dance =
    let%olwt dance = Lwt.return for_dance in
    let%lwt dance = Dance.slug dance in
    Lwt.return_some dance
  in
  Lwt.return
    (make
      ?instruments
      ?forced_pages
      ?show_deviser
      ?show_order
      ?display_name
      ?for_dance
      ?every_version
      ())
