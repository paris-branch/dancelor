open Nes
module Database = Dancelor_server_database

include Dancelor_common_model.SetParameters

let for_dance p =
  let%olwt dance_slug = Lwt.return (for_dance p) in
  let%lwt dance = Dance.get dance_slug in
  Lwt.return_some dance

let make
    ?forced_pages
    ?show_deviser
    ?show_order
    ?display_name
    ?for_dance
    ?every_version
    ()
  =
  let for_dance = Option.map Database.Entry.slug for_dance in
  Lwt.return
    (
      make
        ?forced_pages
        ?show_deviser
        ?show_order
        ?display_name
        ?for_dance
        ?every_version
        ()
    )
