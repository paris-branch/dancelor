open Nes

module Build (Getters : Getters.S) = struct
  include Core.VersionParameters

  let for_dance p =
    let%olwt dance_id = lwt (for_dance p) in
    let%lwt dance = Getters.get_dance dance_id in
    lwt_some dance
end
