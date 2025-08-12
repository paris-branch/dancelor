open NesUnix
open Common

module Ly = Ly
module Pdf = Pdf

let get env id =
  match%lwt Database.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get env book;%lwt
    lwt book

let create env book =
  Permission.assert_can_create env;%lwt
  Database.Book.create book

let update env id book =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Book.update id book

include Search.Build(struct
  type value = Model.Book.t Entry.t
  type filter = Filter.Book.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Book.get_all ()

  let filter_accepts = Filter.Book.accepts

  let tiebreakers =
    Lwt_list.[decreasing (lwt % Model.Book.date') (Option.compare PartialDate.compare);
    increasing (lwt % Model.Book.title') String.Sensible.compare;
    increasing (lwt % Model.Book.title') String.compare_lengths;
    increasing (lwt % Model.Book.subtitle') String.Sensible.compare;
    increasing (lwt % Model.Book.subtitle') String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Pdf -> Pdf.get env
