open Nes
open Common

let get env slug =
  Lwt.bind_return
    (Model.Tune.get slug)
    (Permission.assert_can_get env)

let create env tune =
  Permission.assert_can_create env;%lwt
  Database.Tune.create tune

let update env slug tune =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Tune.update slug tune

include Search.Build(struct
  type value = Model.Tune.t Entry.t
  type filter = Filter.Tune.t

  let get_all env =
    Lwt.map
      (List.filter (Permission.can_get env))
      (Database.Tune.get_all ())

  let filter_accepts = Filter.Tune.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.return % Model.Tune.name') String.Sensible.compare;
    increasing (Lwt.return % Model.Tune.name') String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
