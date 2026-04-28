open Nes
open Dancelor_common

module Version_sql = Version_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Version.t
type entry = Model_builder.Core.Version.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Version.of_yojson Model_builder.Core.Version.access_of_yojson json

let get id : Model_builder.Core.Version.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Option.map (of_json id) <$> Version_sql.get db ~id: (Entry.Id.to_string id)

let get_all () =
  Connection.with_ @@ fun db ->
  Version_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create version =
  let%lwt id = Globally_unique_id.make Version in
  let version = Entry.make ~id ~access: Entry.Access.Public version in
  let json = Entry.to_yojson_no_id Model_builder.Core.Version.to_yojson Model_builder.Core.Version.access_to_yojson version in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Version_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt version

let update id version =
  let version = Entry.make ~id ~access: Entry.Access.Public version in
  let json = Entry.to_yojson_no_id Model_builder.Core.Version.to_yojson Model_builder.Core.Version.access_to_yojson version in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Version_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt version

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Version_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
