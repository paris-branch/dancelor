open Nes
open Common

include ModelBuilder.Person.Build ()

let get = Database.Person.get

let create = Database.Person.create
let update = Database.Person.update
let save = Database.Person.save

include ModelBuilder.Search.Build(struct
    type value = t Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Person.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare
      ]
  end)
