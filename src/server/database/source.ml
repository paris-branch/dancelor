open Nes
open Dancelor_common
open Model_new

module Source_sql = Source_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Source.t
type entry = Model_builder.Core.Source.entry

let sql_to_short_name ~id ~name ~short_name ~(k : Source_short_name.t -> 'w) : 'w =
  let short_name = Option.value short_name ~default: name in
  k {id = Entry.Id.of_string_exn id; short_name}

let sql_to_row ~id ~name ~date ~editors ~(k : Source_row.t -> 'w) : 'w =
  k {id = Entry.Id.of_string_exn id; name; date = Option.map (Option.get % PartialDate.from_string) date; editors}

let search ?(threshold = 0.3) needle : (Source_row.t * float) list Lwt.t =
  Connection.with_ @@ fun db ->
  let%lwt editors = Utils.fold_to_hashtbl Source_sql.Fold.get_all_editors_new db (fun k ~source_id -> Utils.sql_to_person_name ~k: (k source_id)) in
  Source_sql.List.search
    db
    ~needle: (match needle with None -> `None | Some s -> `Some (NEString.to_string s))
    ~threshold: (string_of_float threshold)
    (fun ~score ~id -> sql_to_row ~id ~editors: (Hashtbl.find_all editors id) ~k: (Pair.snoc @@ float_of_string score))

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
    ~id: (Entry.Id.of_string_exn id)
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
  let id = Entry.Id.to_string id in
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
      ignore <$> add_one_editor ~source_id: id ~person_id: (Entry.Id.to_string person_id)
    )
    (Model_builder.Core.Source.editors source)

let get id : Model_builder.Core.Source.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt editors = Source_sql.List.get_editors db ~source_id: id (fun ~person_id -> Entry.Id.of_string_exn person_id) in
  Source_sql.Single.get db ~id (sql_to_source ~id ~editors)

let get_all () =
  Connection.with_ @@ fun db ->
  let editors = Hashtbl.create 8 in
  Source_sql.Fold.get_all_editors db (fun ~source_id ~person_id () -> Hashtbl.add editors source_id (Entry.Id.of_string_exn person_id)) ();%lwt
  Source_sql.List.get_all db (fun ~id -> sql_to_source ~id ~editors: (List.rev @@ Hashtbl.find_all editors id))

let create source =
  Connection.with_ @@ fun db ->
  let%lwt id = Globally_unique_id.make db Source in
  source_to_sql
    ~create_or_update: (Source_sql.create db)
    ~delete_all_editors: (fun ~source_id: _ -> lwt_unit)
    ~add_one_editor: (Source_sql.add_one_editor db)
    id
    source;%lwt
  lwt id

let update id source =
  Connection.with_ @@ fun db ->
  source_to_sql
    ~create_or_update: (fun ~id -> Source_sql.update db ~id)
    ~delete_all_editors: (Source_sql.delete_all_editors db)
    ~add_one_editor: (Source_sql.add_one_editor db)
    id
    source

let delete id =
  Connection.with_ @@ fun db ->
  ignore <$> Source_sql.delete_all_editors ~source_id: (Entry.Id.to_string id) db;%lwt
  ignore <$> Source_sql.delete db ~id: (Entry.Id.to_string id)

let with_cover id f =
  let%lwt cover =
    Connection.with_ @@ fun db ->
    Option.join <$> Source_sql.get_cover db ~id: (Entry.Id.to_string id)
  in
  match cover with
  | None -> f None
  | Some cover ->
    Lwt_io.with_temp_file (fun (fname, ochan) ->
      Lwt_io.write ochan cover;%lwt
      f (Some fname)
    )
