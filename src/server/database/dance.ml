open Nes
open Dancelor_common

module Dance_sql = Dance_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Dance.of_yojson Model_builder.Core.Dance.access_of_yojson json

let get id : Model_builder.Core.Dance.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Option.map (of_json id) <$> Dance_sql.get db ~id: (Entry.Id.to_string id)

let get_all () =
  Connection.with_ @@ fun db ->
  Dance_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create dance =
  let%lwt id = Globally_unique_id.make Dance in
  let dance = Entry.make ~id ~access: Entry.Access.Public dance in
  let json = Entry.to_yojson_no_id Model_builder.Core.Dance.to_yojson Model_builder.Core.Dance.access_to_yojson dance in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Dance_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt dance

let update id dance =
  let dance = Entry.make ~id ~access: Entry.Access.Public dance in
  let json = Entry.to_yojson_no_id Model_builder.Core.Dance.to_yojson Model_builder.Core.Dance.access_to_yojson dance in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Dance_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt dance

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    Dance_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
