open Nes
open Common

include ModelBuilder.Version.Build(Source)(Person)(Tune)

let get = Database.Version.get

module Parameters = struct
  include ModelBuilder.Version.Parameters

  let for_dance p =
    let%olwt dance_slug = Lwt.return (for_dance p) in
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance
end
