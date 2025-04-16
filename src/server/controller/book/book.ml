open NesUnix
open Common

module Ly = Ly
module Pdf = Pdf

let get = Model.Book.get

let create = Database.Book.create
let update = Database.Book.update
let save = Database.Book.save

include ModelBuilder.Search.Build(struct
  type value = Model.Book.t Entry.t
  type filter = Model.Book.Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Book.get_all
  let filter_accepts = Model.Book.Filter.accepts

  let tiebreakers =
    Lwt_list.[decreasing (Lwt.return % Model.Book.date) (Option.compare PartialDate.compare);
    increasing (Lwt.return % Model.Book.title) String.Sensible.compare;
    increasing (Lwt.return % Model.Book.title) String.compare_lengths;
    increasing (Lwt.return % Model.Book.subtitle) String.Sensible.compare;
    increasing (Lwt.return % Model.Book.subtitle) String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Search -> search
  | Create -> create
  | Update -> update
  | Pdf -> Pdf.get
