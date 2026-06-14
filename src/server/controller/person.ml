open Nes
open Dancelor_common
open Model_new
open Search_new

include Shared.Make(struct
  type id = Person_id.t
  type row = Person_row.t
  type view = Person_view.t
  type query = Person_query.t
  include Database.Person
end)

let for_user env id =
  match%lwt Database.Person.get_row_for_user id with
  | None -> lwt_none
  | Some person ->
    Permission.assert_can_get_public_new env person;%lwt
    lwt_some person

(* Legacy *)

(* FIXME: The following conversion functions is temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)
let to_name (person : Model.Person.entry) : Person_name.t = {
  Person_name.id = Entry.id person;
  name = NEString.to_string @@ Model.Person.name' person;
}

let get env id =
  match%lwt Database.Person.get id with
  | None -> Permission.reject_can_get ()
  | Some person ->
    Permission.assert_can_get_public env person;%lwt
    lwt person

let create env person =
  Permission.assert_can_create_public env;%lwt
  Database.Person.create person

let update env id person =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Person.update id person

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Person.delete id

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Person.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | For_user -> for_user env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
