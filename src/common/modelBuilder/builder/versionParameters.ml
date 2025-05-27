open Nes

module Build (Getters : Getters.S) = struct
  include Core.VersionParameters

  let for_dance p =
    let%olwt dance_slug = lwt (for_dance p) in
    let%lwt dance = Getters.get_dance dance_slug in
    lwt_some dance
end
