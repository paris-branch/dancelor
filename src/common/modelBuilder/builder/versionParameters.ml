module Build (Getters : Getters.S) = struct
  include Core.VersionParameters

  let for_dance p =
    let%olwt dance_slug = Lwt.return (for_dance p) in
    let%lwt dance = Getters.get_dance dance_slug in
    Lwt.return_some dance
end
