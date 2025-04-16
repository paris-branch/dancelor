open Nes
open Common

let get = Model.Person.get

let create = Database.Person.create
let update = Database.Person.update
let save = Database.Person.save

include ModelBuilder.Search.Build(struct
  type value = Model.Person.t Entry.t
  type filter = Model.Person.Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Person.get_all
  let filter_accepts = Model.Person.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Person.name) String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Search -> search
  | Create -> create
  | Update -> update
