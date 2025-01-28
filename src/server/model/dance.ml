open Nes
open Dancelor_common
module Database = Dancelor_server_database

include ModelBuilder.Dance.Build(Person)

let get = Dancelor_server_database.Dance.get

let create = Database.Dance.create
let update = Database.Dance.update
let save = Database.Dance.save

include ModelBuilder.Search.Build(struct
    type value = t Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Dance.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare
      ]
  end)
