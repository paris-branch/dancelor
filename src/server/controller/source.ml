open Nes
open Dancelor_common
open Model_new
open Search_new

include Shared.Make(struct
  type id = Source_id.t
  type row = Source_row.t
  type view = Source_view.t
  type query = Source_query.t
  include Database.Source
end)

(* Legacy *)

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)
let to_name (source : Model.Source.entry) : Source_name.t = {
  Source_name.id = Entry.id source;
  name = NEString.to_string @@ Model.Source.name' source;
}
let to_short_name (source : Model.Source.entry) : Source_short_name.t = {
  Source_short_name.id = Entry.id source;
  short_name =
  NEString.to_string (
    match Model.Source.short_name' source with
    | None -> Model.Source.name' source
    | Some name -> name
  );
}

let get env id =
  match%lwt Database.Source.get id with
  | None -> Permission.reject_can_get ()
  | Some source ->
    Permission.assert_can_get_public env source;%lwt
    lwt source

let create env source =
  Permission.assert_can_create_public env;%lwt
  Database.Source.create source

let update env id source =
  Permission.assert_can_update_public env =<< get env id;%lwt
  Database.Source.update id source

let delete env id =
  Permission.assert_can_delete_public env =<< get env id;%lwt
  Database.Source.delete id

let get_cover env id =
  Permission.assert_can_get_public env =<< get env id;%lwt
  Database.Source.with_cover id @@ fun fname ->
  let fname = Option.value fname ~default: (Filename.concat (Config.get ()).share "no-cover.webp") in
  Madge_server.respond_file ~fname

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Source.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Cover -> get_cover env
