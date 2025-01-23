open Nes
module Common = Dancelor_common
module Database = Dancelor_server_database

include PersonLifted

let create = Database.Person.create
let update = Database.Person.update
let save = Database.Person.save

include Common.Model.Search.Make(struct
    type value = t Database.Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Person.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.return % name) String.Sensible.compare
      ]
  end)
