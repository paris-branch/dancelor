open Nes
open Dancelor_common

module Dance_sql = Dance_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Dance.of_yojson Model_builder.Core.Dance.access_of_yojson json

let get id : Model_builder.Core.Dance.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Dance_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Dance_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create dance =
  let%lwt id = Globally_unique_id.make Dance in
  let dance = Entry.make ~id ~access: Entry.Access.Public dance in
  let json = Entry.to_yojson_no_id Model_builder.Core.Dance.to_yojson Model_builder.Core.Dance.access_to_yojson dance in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Dance_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt dance

let update id dance =
  let dance = Entry.make ~id ~access: Entry.Access.Public dance in
  let json = Entry.to_yojson_no_id Model_builder.Core.Dance.to_yojson Model_builder.Core.Dance.access_to_yojson dance in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Dance_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt dance

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Dance_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
