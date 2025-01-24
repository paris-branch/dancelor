open Nes
open Dancelor_common
module Database = Dancelor_server_database

include Model.Tune.Lift(Dance)(Person)

let get = Dancelor_server_database.Tune.get

let create = Database.Tune.create
let update = Database.Tune.update
let save = Database.Tune.save

include Model.Search.Make(struct
    type value = t Database.Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Tune.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare;
        increasing (Lwt.return % name) String.compare_lengths;
      ]
  end)
