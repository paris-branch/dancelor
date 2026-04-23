open Nes
open Dancelor_common

module Set_sql = Set_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Set.of_yojson Model_builder.Core.Set.access_of_yojson json

let get id : Model_builder.Core.Set.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Set_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Set_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create set access =
  let%lwt id = Globally_unique_id.make Set in
  let set = Entry.make ~id ~access set in
  let json = Entry.to_yojson_no_id Model_builder.Core.Set.to_yojson Model_builder.Core.Set.access_to_yojson set in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Set_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt set

let update id set access =
  let set = Entry.make ~id ~access set in
  let json = Entry.to_yojson_no_id Model_builder.Core.Set.to_yojson Model_builder.Core.Set.access_to_yojson set in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Set_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt set

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Set_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
