open Nes
open Dancelor_common

module Source_sql = Source_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

let of_json id json =
  Result.get_ok @@ Entry.of_yojson_no_id id Model_builder.Core.Source.of_yojson Model_builder.Core.Source.access_of_yojson json

let get id : Model_builder.Core.Source.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  lwt @@ Option.map (of_json id) (Source_sql.get db ~id: (Entry.Id.to_string id))

let get_all () =
  Connection.with_ @@ fun db ->
  lwt @@ Source_sql.List.get_all db (fun ~id ~json -> of_json (Entry.Id.of_string_exn id) json)

let create source =
  let%lwt id = Globally_unique_id.make Source in
  let source = Entry.make ~id ~access: Entry.Access.Public source in
  let json = Entry.to_yojson_no_id Model_builder.Core.Source.to_yojson Model_builder.Core.Source.access_to_yojson source in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Source_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt source

let update id source =
  let source = Entry.make ~id ~access: Entry.Access.Public source in
  let json = Entry.to_yojson_no_id Model_builder.Core.Source.to_yojson Model_builder.Core.Source.access_to_yojson source in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Source_sql.update db ~id: (Entry.Id.to_string id) ~json
  in
  lwt source

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    lwt @@ Source_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit

let with_cover id f =
  let%lwt cover =
    Connection.with_ @@ fun db ->
    lwt @@ Option.join (Source_sql.get_cover db ~id: (Entry.Id.to_string id))
  in
  match cover with
  | None -> f None
  | Some cover ->
    Lwt_io.with_temp_file (fun (fname, ochan) ->
      Lwt_io.write ochan cover;%lwt
      f (Some fname)
    )
