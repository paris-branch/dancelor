open Nes

module Build (Getters : Getters.S) = struct
  include Core.VersionParameters

  let make ?instruments ?transposition ?clef ?first_bar ?display_name ?display_composer ?for_dance () =
    let for_dance = Option.map Entry.id for_dance in
    make ?instruments ?transposition ?clef ?first_bar ?display_name ?display_composer ?for_dance ()

  let for_dance p =
    let%olwt dance_id = lwt (for_dance p) in
    let%lwt dance = Option.get <$> Getters.get_dance dance_id in
    lwt_some dance
end
