open Nes
open Dancelor_common

module Tune_sql = Tune_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Tune.of_yojson Model_builder.Core.Tune.access_of_yojson json

let get id : Model_builder.Core.Tune.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Tune_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Tune_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create tune =
  let id = Table.Globally_unique_id.make () in
  let tune = Entry.make ~id ~access: Entry.Access.Public tune in
  let json = Entry.to_yojson_no_id Model_builder.Core.Tune.to_yojson Model_builder.Core.Tune.access_to_yojson tune in
  Table.Globally_unique_id.register tune ~wrap_any: Model_builder.Core.Any.tune;
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Tune_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt tune

let update id tune =
  let tune = Entry.make ~id ~access: Entry.Access.Public tune in
  let json = Entry.to_yojson_no_id Model_builder.Core.Tune.to_yojson Model_builder.Core.Tune.access_to_yojson tune in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Tune_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt tune

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Tune_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
