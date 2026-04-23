open Nes
open Dancelor_common

module Person_sql = Person_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Person.t
type entry = Model_builder.Core.Person.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Person.of_yojson Model_builder.Core.Person.access_of_yojson json

let get id : Model_builder.Core.Person.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Person_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Person_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create person =
  let%lwt id = Globally_unique_id.make Person in
  let person = Entry.make ~id ~access: Entry.Access.Public person in
  let json = Entry.to_yojson_no_id Model_builder.Core.Person.to_yojson Model_builder.Core.Person.access_to_yojson person in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Person_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt person

let update id person =
  let person = Entry.make ~id ~access: Entry.Access.Public person in
  let json = Entry.to_yojson_no_id Model_builder.Core.Person.to_yojson Model_builder.Core.Person.access_to_yojson person in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Person_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt person

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Person_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
