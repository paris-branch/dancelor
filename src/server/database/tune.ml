open Nes
open Dancelor_common

module Tune_sql = Tune_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Tune.of_yojson Model_builder.Core.Tune.access_of_yojson json

let get id : Model_builder.Core.Tune.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Option.map (of_json id) <$> Tune_sql.get db ~id: (Entry.Id.to_string id)

let get_all () =
  Connection.with_ @@ fun db ->
  Tune_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create tune =
  let%lwt id = Globally_unique_id.make Tune in
  let tune = Entry.make ~id ~access: Entry.Access.Public tune in
  let json = Entry.to_yojson_no_id Model_builder.Core.Tune.to_yojson Model_builder.Core.Tune.access_to_yojson tune in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Tune_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt tune

let update id tune =
  let tune = Entry.make ~id ~access: Entry.Access.Public tune in
  let json = Entry.to_yojson_no_id Model_builder.Core.Tune.to_yojson Model_builder.Core.Tune.access_to_yojson tune in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Tune_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt tune

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Tune_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
