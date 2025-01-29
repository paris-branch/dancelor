open Nes
open Common

include ModelBuilder.Tune.Build(Dance)(Person)

let get = Database.Tune.get

let create = Database.Tune.create
let update = Database.Tune.update
let save = Database.Tune.save

include ModelBuilder.Search.Build(struct
    type value = t Entry.t
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
