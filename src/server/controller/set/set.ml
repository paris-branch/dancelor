open Nes
open Common

module Pdf = Pdf

let get env slug =
  let%lwt set = Model.Set.get slug in
  Permission.assert_can_get env set;%lwt
  lwt set

let create env set =
  Permission.assert_can_create env;%lwt
  Database.Set.create set

let update env slug set =
  Permission.assert_can_update env =<< get env slug;%lwt
  Database.Set.update slug set

let delete env slug =
  Permission.assert_can_delete env =<< get env slug;%lwt
  Database.Set.delete slug

include Search.Build(struct
  type value = Model.Set.t Entry.t
  type filter = Filter.Set.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Set.get_all ()

  let filter_accepts = Filter.Set.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % Model.Set.name') String.Sensible.compare;
    increasing (lwt % Model.Set.name') String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Pdf -> Pdf.get env
