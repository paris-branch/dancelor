open Nes
open Dancelor_common

module Source_sql = Source_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

let of_yaml id yaml =
  let json = Storage.Json.from_yaml_string yaml in
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Source.of_yojson Model_builder.Core.Source.access_of_yojson json

let get id : Model_builder.Core.Source.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  Source_sql.Fold.get db ~id: (Entry.Id.to_string id) (fun ~yaml _ -> Some (of_yaml id yaml)) None

let get_all () =
  Connection.with_ @@ fun db ->
  Source_sql.List.get_all db (fun ~id ~yaml -> of_yaml (Entry.Id.of_string_exn id) yaml)

let create source =
  let id = Table.Globally_unique_id.make () in
  let source = Entry.make ~id ~access: Entry.Access.Public source in
  let json = Entry.to_yojson_no_id Model_builder.Core.Source.to_yojson Model_builder.Core.Source.access_to_yojson source in
  Table.Globally_unique_id.register source ~wrap_any: Model_builder.Core.Any.source;
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Source_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt source

let update id source =
  let source = Entry.make ~id ~access: Entry.Access.Public source in
  let json = Entry.to_yojson_no_id Model_builder.Core.Source.to_yojson Model_builder.Core.Source.access_to_yojson source in
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Source_sql.update db ~id: (Entry.Id.to_string id) ~yaml: (Storage.Json.to_yaml_string json)
  in
  lwt source

let delete id =
  let%lwt _ : int64 =
    Connection.with_ @@ fun db ->
    Source_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit

let with_cover id f =
  Connection.with_ @@ fun db ->
  let%lwt cover = Source_sql.Fold.get_cover db ~id: (Entry.Id.to_string id) (fun ~cover _ -> cover) None in
  match cover with
  | None -> f None
  | Some cover ->
    Lwt_io.with_temp_file (fun (fname, ochan) ->
      Lwt_io.write ochan cover;%lwt
      f (Some fname)
    )
