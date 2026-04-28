open Nes
open Dancelor_common

module Set_sql = Set_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Set.t
type entry = Model_builder.Core.Set.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Set.of_yojson Model_builder.Core.Set.access_of_yojson json

let get id : Model_builder.Core.Set.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Option.map (of_json id) <$> Set_sql.get db ~id: (Entry.Id.to_string id)

let get_all () =
  Connection.with_ @@ fun db ->
  Set_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create set access =
  let%lwt id = Globally_unique_id.make Set in
  let set = Entry.make ~id ~access set in
  let json = Entry.to_yojson_no_id Model_builder.Core.Set.to_yojson Model_builder.Core.Set.access_to_yojson set in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Set_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt set

let update id set access =
  let set = Entry.make ~id ~access set in
  let json = Entry.to_yojson_no_id Model_builder.Core.Set.to_yojson Model_builder.Core.Set.access_to_yojson set in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Set_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt set

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Set_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
