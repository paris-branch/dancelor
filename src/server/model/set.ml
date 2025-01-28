open Nes
open Dancelor_common
module Database = Dancelor_server_database

include Model.Set.Lift(Dance)(Person)(Tune)(Version)

let get = Dancelor_server_database.Set.get

let create = Database.Set.create
let update = Database.Set.update
let save = Database.Set.save

let delete = Database.(Set.delete % Entry.slug)

include Model.Search.Make(struct
    type value = t Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Set.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare;
        increasing (Lwt.return % name) String.compare_lengths;
      ]
  end)

module Parameters = struct
  include Dancelor_common.Model.SetParameters

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
end
