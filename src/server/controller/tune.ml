open Nes
open Dancelor_common

let get env id =
  match%lwt Database.Tune.get id with
  | None -> Permission.reject_can_get ()
  | Some tune ->
    Permission.assert_can_get_public env tune;%lwt
    lwt tune

let create env tune =
  Permission.assert_can_create_public env;%lwt
  Database.Tune.create tune

let update env id tune =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Tune.update id tune

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Tune.delete id

include Search.Build(struct
  type value = Model.Tune.entry
  type filter = (Model.Tune.t, Filter.Tune.t) Formula_entry.public

  let get_all env =
    let all = Database.Tune.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_public env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_public Filter.Tune.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_public Filter.Tune.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.Tune.one_name') String.Sensible.compare;
    increasing (lwt % NEString.to_string % Model.Tune.one_name') String.compare_lengths;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
