open Nes
open Common

module Pdf = Pdf

let get env id =
  let%lwt set = Model.Set.get id in
  Permission.assert_can_get env set;%lwt
  lwt set

let create env set =
  Permission.assert_can_create env;%lwt
  Database.Set.create set

let update env id set =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Set.update id set

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Set.delete id

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
