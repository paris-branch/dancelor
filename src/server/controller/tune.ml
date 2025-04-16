open Nes
open Common

let get = Model.Tune.get

let create = Database.Tune.create
let update = Database.Tune.update
let save = Database.Tune.save

include ModelBuilder.Search.Build(struct
  type value = Model.Tune.t Entry.t
  type filter = Model.Tune.Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Tune.get_all
  let filter_accepts = Model.Tune.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Tune.name) String.Sensible.compare;
    increasing (Lwt.return % Model.Tune.name) String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Search -> search
  | Create -> create
  | Update -> update
