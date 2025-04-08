open Nes
open Common

include ModelBuilder.Source.Build ()

let get = Database.Source.get

let create = Database.Source.create
let update = Database.Source.update
let save = Database.Source.save

include ModelBuilder.Search.Build(struct
    type value = t Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Source.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare
      ]
  end)
