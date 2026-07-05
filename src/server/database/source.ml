open Nes
open Dancelor_common
open Model_new
open Search_new
open Sql_to_name
open Sql_to_row
open Sql_to_view

module Source_sql = Source_sql.Sqlgg(Sqlgg_postgresql)

let get_editors_for db source_ids =
  Utils.fold_to_get_list (Source_sql.Fold.get_editors_for db ~source_ids) (fun k ~source_id -> person_sql_to_name ~k: (k source_id))

let get_row_for ids : (Source_id.t -> Source_row.t option) Lwt.t =
  Connection.with_ @@ fun db ->
  let%lwt editors_for = get_editors_for db (`One_of ids) in
  Utils.fold_to_get_single (Source_sql.Fold.get_rows db ~ids) (fun k ~id -> source_sql_to_row ~id ~editors: (editors_for id) ~k: (k id))

let get_view id : Source_view.t option Lwt.t =
  Connection.with_ @@ fun db ->
  let%lwt editors = (fun f -> f id) <$> get_editors_for db (`One_of [id]) in
  Source_sql.Single.get_view db ~id (source_sql_to_view ~editors ~k: Fun.id)

let search query : (Source_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = {Source_query.editor}} = query in
  Connection.with_ @@ fun db ->
  let%lwt editors_for = get_editors_for db `All in
  Source_sql.List.search
    db
    ~terms
    ~editor: (Utils.option_to_sql editor)
    (fun ~score ~id -> source_sql_to_row ~id ~editors: (editors_for id) ~k: (Pair.snoc score))

(* Legacy *)

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

let sql_to_source
    ~id
    ~name
    ~short_name
    ~scddb_id
    ~description
    ~date
    ~editors
    ~created_at
    ~modified_at
  =
  Entry.make
    ~id
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Source.make
        ~name: (NEString.of_string_exn name)
        ~short_name: (Option.map NEString.of_string_exn short_name)
        ~scddb_id: (Option.map Int64.to_int scddb_id)
        ~description
        ~date: (Option.map (Option.get % PartialDate.from_string) date)
        ~editors
        ()
    )

let source_to_sql ~create_or_update ~delete_all_editors ~add_one_editor id source =
  (* FIXME: transaction, maybe [Connection.with_transaction] *)
  ignore
  <$> create_or_update
      ~id
      ~name: (NEString.to_string @@ Model_builder.Core.Source.name source)
      ~short_name: (Option.map NEString.to_string @@ Model_builder.Core.Source.short_name source)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Source.scddb_id source)
      ~description: (Model_builder.Core.Source.description source)
      ~date: (Option.map PartialDate.to_string @@ Model_builder.Core.Source.date source);%lwt
  ignore <$> delete_all_editors ~source_id: id;%lwt
  Lwt_list.iter_s
    (fun person_id ->
      ignore <$> add_one_editor ~source_id: id ~person_id
    )
    (Model_builder.Core.Source.editors source)

let get id : Model_builder.Core.Source.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  let%lwt editors = Source_sql.List.get_editors db ~source_id: id (fun ~person_id -> person_id) in
  Source_sql.Single.get db ~id (sql_to_source ~id ~editors)

let create source =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make_public db `Source in
  source_to_sql
    ~create_or_update: (Source_sql.create db)
    ~delete_all_editors: (fun ~source_id: _ -> lwt_unit)
    ~add_one_editor: (Source_sql.add_one_editor db)
    id
    source;%lwt
  lwt id

let update id source =
  Connection.with_ @@ fun db ->
  Entry_new.touch db id;%lwt
  source_to_sql
    ~create_or_update: (fun ~id -> Source_sql.update db ~id)
    ~delete_all_editors: (Source_sql.delete_all_editors db)
    ~add_one_editor: (Source_sql.add_one_editor db)
    id
    source

let delete id =
  Connection.with_ @@ fun db ->
  ignore <$> Source_sql.delete_all_editors ~source_id: id db;%lwt
  ignore <$> Source_sql.delete db ~id;%lwt
  Entry_new.delete db id

let with_cover id f =
  let%lwt cover =
    Connection.with_ @@ fun db ->
    Option.join <$> Source_sql.get_cover db ~id
  in
  match cover with
  | None -> f None
  | Some cover ->
    let cover = Postgresql.unescape_bytea cover in
    Lwt_io.with_temp_file (fun (fname, ochan) ->
      Lwt_io.write ochan cover;%lwt
      f (Some fname)
    )
