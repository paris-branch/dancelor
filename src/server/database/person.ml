open Nes
open Dancelor_common

module Person_sql = Person_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Person.of_yojson Model_builder.Core.Person.access_of_yojson json

let get id : Model_builder.Core.Person.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  lwt @@ Option.map (of_json id) (Person_sql.get db ~id: (Entry.Id.to_string id))

let get_all () =
  Connection.with_ @@ fun db ->
  lwt @@ Person_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create person =
  let%lwt id = Globally_unique_id.make Person in
  let person = Entry.make ~id ~access: Entry.Access.Public person in
  let json = Entry.to_yojson_no_id Model_builder.Core.Person.to_yojson Model_builder.Core.Person.access_to_yojson person in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Person_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt person

let update id person =
  let person = Entry.make ~id ~access: Entry.Access.Public person in
  let json = Entry.to_yojson_no_id Model_builder.Core.Person.to_yojson Model_builder.Core.Person.access_to_yojson person in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Person_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt person

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Person_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
