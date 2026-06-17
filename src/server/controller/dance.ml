open NesUnix
open Dancelor_common
open Model_new
open Search_new

include Shared.Make(struct
  type id = Dance_id.t
  type row = Dance_row.t
  type view = Dance_view.t
  type query = Dance_query.t
  include Database.Dance
end)

(* Legacy *)

(* FIXME: The following conversion function is temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)
let to_row (dance : Model.Dance.entry) : Dance_row.t Lwt.t =
  let%lwt devisers = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Dance.devisers' dance in
  let devisers = List.map Person.to_name devisers in
  lwt {
    Dance_row.id = Entry.id dance;
    name = NEString.to_string @@ NEList.hd @@ Model.Dance.names' dance;
    kind = Model.Dance.kind' dance;
    devisers;
    disambiguation = Option.map NEString.to_string @@ Model.Dance.disambiguation' dance;
  }

let get env id =
  match%lwt Database.Dance.get id with
  | None -> Permission.reject_can_get ()
  | Some dance ->
    Permission.assert_can_get_public env dance;%lwt
    lwt dance

let create env dance =
  Permission.assert_can_create_public env;%lwt
  Database.Dance.create dance

let update env id dance =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Dance.update id dance

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Dance.delete id

let tunes env id =
  let%lwt _ = get env id in
  let%lwt tunes = Database.Tune.get_rows_for_dance id in
  let%lwt tunes = Lwt_list.filter_s (Permission.can_get_public_new env) tunes in
  lwt tunes

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Dance.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Tunes -> tunes env
