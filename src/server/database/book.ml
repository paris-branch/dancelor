open Nes
open Dancelor_common

module Book_sql = Book_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Book.t
type entry = Model_builder.Core.Book.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Book.of_yojson Model_builder.Core.Book.access_of_yojson json

let get id : Model_builder.Core.Book.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Book_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Book_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create book access =
  let%lwt id = Globally_unique_id.make Book in
  let book = Entry.make ~id ~access book in
  let json = Entry.to_yojson_no_id Model_builder.Core.Book.to_yojson Model_builder.Core.Book.access_to_yojson book in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Book_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt book

let update id book access =
  let book = Entry.make ~id ~access book in
  let json = Entry.to_yojson_no_id Model_builder.Core.Book.to_yojson Model_builder.Core.Book.access_to_yojson book in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Book_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt book

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Book_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit
