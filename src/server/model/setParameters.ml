open Nes
open Common

include ModelBuilder.SetParameters

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
  let for_dance = Option.map Entry.slug for_dance in
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
