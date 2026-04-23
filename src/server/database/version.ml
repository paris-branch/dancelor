open Nes
open Dancelor_common

module Version_sql = Version_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Version.t
type entry = Model_builder.Core.Version.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Version.of_yojson Model_builder.Core.Version.access_of_yojson json

let get id : Model_builder.Core.Version.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Version_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Version_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create version =
  let%lwt id = Globally_unique_id.make Version in
  let version = Entry.make ~id ~access: Entry.Access.Public version in
  let json = Entry.to_yojson_no_id Model_builder.Core.Version.to_yojson Model_builder.Core.Version.access_to_yojson version in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Version_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt version

let update id version =
  let version = Entry.make ~id ~access: Entry.Access.Public version in
  let json = Entry.to_yojson_no_id Model_builder.Core.Version.to_yojson Model_builder.Core.Version.access_to_yojson version in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Version_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt version

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Version_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
