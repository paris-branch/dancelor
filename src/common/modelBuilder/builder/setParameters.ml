open Nes

module Build (Getters : Getters.S) = struct
  include Core.SetParameters

  let make' ?forced_pages ?show_deviser ?show_order ?display_name ?for_dance ?every_version () =
    let for_dance = Option.map Entry.slug for_dance in
    lwt @@ make ?forced_pages ?show_deviser ?show_order ?display_name ?for_dance ?every_version ()

  let for_dance p =
    let%olwt dance_slug = lwt (for_dance p) in
    let%lwt dance = Getters.get_dance dance_slug in
    lwt_some dance
end
