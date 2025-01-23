open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include SetLifted

let create = Database.Set.create
let update = Database.Set.update
let save = Database.Set.save

let delete = Database.(Set.delete % Entry.slug)

include Common.Model.Search.Make(struct
    type value = t Database.Entry.t
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
