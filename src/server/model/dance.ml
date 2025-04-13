open Nes
open Common

include ModelBuilder.Dance.Build(Person)

let get = Database.Dance.get

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
    Lwt_list.[increasing (Lwt.return % name) String.Sensible.compare]
end)
