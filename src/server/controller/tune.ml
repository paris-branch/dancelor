open Nes
open Dancelor_common
open Model_new
open Search_new

include Shared.Make_public(struct
  type id = Tune_id.t
  type row = Tune_row.t
  type view = Tune_view.t
  type query = Tune_query.t
  include Database.Tune
end)

(* Legacy *)

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
  ignore <$> Database.Tune.update id tune

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Tune.delete id

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Tune.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
