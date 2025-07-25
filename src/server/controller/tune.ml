open Nes
open Common

let get env id =
  let%lwt tune = Model.Tune.get id in
  Permission.assert_can_get env tune;%lwt
  lwt tune

let create env tune =
  Permission.assert_can_create env;%lwt
  Database.Tune.create tune

let update env id tune =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Tune.update id tune

include Search.Build(struct
  type value = Model.Tune.t Entry.t
  type filter = Filter.Tune.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Tune.get_all ()

  let filter_accepts = Filter.Tune.accepts

  let tiebreakers =
    Lwt_list.[increasing (lwt % Model.Tune.name') String.Sensible.compare;
    increasing (lwt % Model.Tune.name') String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
