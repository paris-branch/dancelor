open NesUnix
open Common

module Ly = Ly
module Pdf = Pdf

let get env slug =
  Lwt.bind_return
    (Model.Book.get slug)
    (Permission.assert_can_get env)

let create env book =
  Permission.assert_can_create env;%lwt
  Database.Book.create book

let update env slug book =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Book.update slug book

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

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search (* FIXME *)
  | Create -> create env
  | Update -> update env
  | Pdf -> Pdf.get env
