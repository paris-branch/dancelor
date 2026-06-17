open Nes
open Dancelor_common
open Model_new
open Search_new

module Tune_sql = Tune_sql.Sqlgg(Sqlgg_postgresql)

type sql_kind_base = [`Jig | `Reel | `Strathspey | `Waltz | `Polka | `Jig_9_8 | `Other]

let sql_to_kind_base : sql_kind_base -> Kind_base.t = function
  | `Jig -> Jig
  | `Reel -> Reel
  | `Strathspey -> Strathspey
  | `Waltz -> Waltz
  | `Polka -> Polka
  | `Jig_9_8 -> Jig_9_8
  | `Other -> Other

let kind_base_to_sql : Kind_base.t -> sql_kind_base = function
  | Jig -> `Jig
  | Reel -> `Reel
  | Strathspey -> `Strathspey
  | Waltz -> `Waltz
  | Polka -> `Polka
  | Jig_9_8 -> `Jig_9_8
  | Other -> `Other

let sql_to_row ~id ~name ~kind ~composers ~(k : Tune_row.t -> 'w) : 'w =
  k {
    id = Entry.Id.of_string_exn id;
    name;
    kind = sql_to_kind_base kind;
    composers;
  }

let sql_to_version_row_without_tune ~id ~sources ~disambiguation ~arrangers ~monolithic_bars ~monolithic_or_default_structure ~(k : Tune_view.version_row_without_tune -> 'w) : 'w =
  let content : Version_row.content =
    match (monolithic_bars, monolithic_or_default_structure) with
    | (None, None) -> No_content
    | (None, Some _default_structure) -> Destructured
    | (Some bars, Some structure) ->
      Monolithic {
        bars = Int64.to_int bars;
        structure = Option.get (Model_builder.Core.Version.Structure.of_string (NEString.of_string_exn structure));
      }
    | _ -> assert false
  in
  k {
    id = Entry.Id.of_string_exn id;
    sources;
    disambiguation;
    arrangers;
    content;
  }

let sql_to_view ~id ~name ~extra_names ~kind ~composers ~dances ~remark ~scddb_id ~date ~versions ~(k : Tune_view.t -> 'w) : 'w =
  k {
    id = Entry.Id.of_string_exn id;
    name;
    extra_names;
    kind = sql_to_kind_base kind;
    composers;
    dances;
    remark;
    scddb_id = Option.map Int64.to_int scddb_id;
    date = Option.map (Option.get % PartialDate.from_string) date;
    versions;
  }

let get_extra_names_for db tune_ids =
  Utils.fold_to_get (Tune_sql.Fold.get_extra_names_for ~tune_ids) db (fun k ~tune_id ~extra_name -> k tune_id extra_name)

let get_dances_for db tune_ids =
  let%lwt devisers_for = Utils.fold_to_get (Tune_sql.Fold.get_devisers_for_dances_of ~tune_ids) db (fun k ~dance_id -> Person.sql_to_name ~k: (k dance_id)) in
  Utils.fold_to_get (Tune_sql.Fold.get_dances_for ~tune_ids) db (fun k ~tune_id ~id -> Dance.sql_to_row ~id ~devisers: (devisers_for id) ~k: (k tune_id))

let get_versions_for db tune_ids =
  let%lwt sources_for = Utils.fold_to_get (Tune_sql.Fold.get_sources_for_versions_of ~tune_ids) db (fun k ~version_id -> Source.sql_to_short_name ~k: (k version_id)) in
  let%lwt arrangers_for = Utils.fold_to_get (Tune_sql.Fold.get_arrangers_for_versions_of ~tune_ids) db (fun k ~version_id -> Person.sql_to_name ~k: (k version_id)) in
  Utils.fold_to_get (Tune_sql.Fold.get_versions_for ~tune_ids) db (fun k ~id ~tune_id -> sql_to_version_row_without_tune ~id ~arrangers: (arrangers_for id) ~sources: (sources_for id) ~k: (k tune_id))

let get_composers_for db tune_ids =
  Utils.fold_to_get (Tune_sql.Fold.get_composers_for ~tune_ids) db (fun k ~tune_id -> Person.sql_to_name ~k: (k tune_id))

let get_composers_with_details_for db tune_ids =
  Utils.fold_to_get (Tune_sql.Fold.get_composers_with_details_for ~tune_ids) db (fun k ~tune_id -> Person.sql_to_name_with_details ~k: (k tune_id))

let get_row id : Tune_row.t option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt composers = (fun f -> f id) <$> get_composers_for db (`One_of [id]) in
  Tune_sql.Single.get_row db ~id (sql_to_row ~id ~composers ~k: Fun.id)

let get_rows ids : (Tune_id.t, Tune_row.t) Utils.tbl Lwt.t =
  let ids = List.map Entry.Id.to_string ids in
  Connection.with_ @@ fun db ->
  let%lwt composers_for = get_composers_for db (`One_of ids) in
  Utils.fold_to_tbl (Tune_sql.Fold.get_rows ~ids) db (fun k ~id -> sql_to_row ~id ~composers: (composers_for id) ~k: (k @@ Entry.Id.of_string_exn id))

let get_rows_for_dance dance_id : Tune_row.t list Lwt.t =
  Connection.with_ @@ fun db ->
  let%lwt composers_for = get_composers_for db `All in
  Tune_sql.List.get_rows_for_dance
    db
    ~dance_id: (Entry.Id.to_string dance_id)
    (fun ~id -> sql_to_row ~id ~composers: (composers_for id) ~k: Fun.id)

let get_view id : Tune_view.t option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt extra_names = (fun f -> f id) <$> get_extra_names_for db (`One_of [id]) in
  let%lwt dances = (fun f -> f id) <$> get_dances_for db (`One_of [id]) in
  let%lwt versions = (fun f -> f id) <$> get_versions_for db (`One_of [id]) in
  let%lwt composers = (fun f -> f id) <$> get_composers_with_details_for db (`One_of [id]) in
  Tune_sql.Single.get_view db ~id (sql_to_view ~extra_names ~dances ~composers ~versions ~id ~k: Fun.id)

let search query : (Tune_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = {Tune_query.kind; composer}} = query in
  Connection.with_ @@ fun db ->
  let%lwt composers_for = get_composers_for db `All in
  Tune_sql.List.search
    db
    ~terms
    ~kind: (Option.map (List.map kind_base_to_sql) kind)
    ~composer: (Utils.list_option_map_to_sql Entry.Id.to_string composer)
    (fun ~score ~id -> sql_to_row ~id ~composers: (composers_for id) ~k: (Pair.snoc score))

(* Legacy *)

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

let sql_to_tune
    ~id
    ~name
    ~extra_names
    ~kind
    ~remark
    ~scddb_id
    ~date
    ~created_at
    ~modified_at
    ~composers
    ~dances
  =
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Tune.make
        ~names: (NEList.cons (NEString.of_string_exn name) extra_names)
        ~kind: (sql_to_kind_base kind)
        ~remark: (Option.map NEString.of_string_exn remark)
        ~scddb_id: (Option.map Int64.to_int scddb_id)
        ~date: (Option.map (Option.get % PartialDate.from_string) date)
        ~composers: (List.map (fun (composer, details) -> {Model_builder.Core.Tune.composer; details}) composers)
        ~dances
        ()
    )

let tune_to_sql ~create_or_update db id tune =
  (* FIXME: transaction, maybe [Connection.with_transaction] *)
  let id = Entry.Id.to_string id in
  ignore
  <$> create_or_update
      db
      ~id
      ~name: (NEString.to_string @@ NEList.hd @@ Model_builder.Core.Tune.names tune)
      ~kind: (kind_base_to_sql @@ Model_builder.Core.Tune.kind tune)
      ~remark: (Option.map NEString.to_string @@ Model_builder.Core.Tune.remark tune)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Tune.scddb_id tune)
      ~date: (Option.map PartialDate.to_string @@ Model_builder.Core.Tune.date tune);%lwt
  ignore <$> Tune_sql.delete_all_extra_names db ~tune_id: id;%lwt
  Lwt_list.iter_s
    (fun extra_name ->
      ignore <$> Tune_sql.add_one_extra_name db ~tune_id: id ~extra_name: (NEString.to_string extra_name)
    )
    (NEList.tl @@ Model_builder.Core.Tune.names tune);%lwt
  ignore <$> Tune_sql.delete_all_composers db ~tune_id: id;%lwt
  Lwt_list.iteri_s
    (fun index {Model_builder.Core.Tune.composer; details} ->
      ignore
      <$> Tune_sql.add_one_composer
          db
          ~tune_id: id
          ~index: (Int64.of_int index)
          ~composer_id: (Entry.Id.to_string composer)
          ~details: (Option.map NEString.to_string details)
    )
    (Model_builder.Core.Tune.composers tune);%lwt
  ignore <$> Tune_sql.delete_all_dances db ~tune_id: id;%lwt
  Lwt_list.iter_s
    (fun dance_id ->
      ignore <$> Tune_sql.add_one_dance db ~tune_id: id ~dance_id: (Entry.Id.to_string dance_id)
    )
    (Model_builder.Core.Tune.dances tune)

let get id : Model_builder.Core.Tune.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt extra_names = Tune_sql.List.get_extra_names db ~tune_id: id (fun ~extra_name -> NEString.of_string_exn extra_name) in
  let%lwt composers = Tune_sql.List.get_composers db ~tune_id: id (fun ~composer_id ~details -> (Entry.Id.of_string_exn composer_id, Option.map NEString.of_string_exn details)) in
  let%lwt dances = Tune_sql.List.get_dances db ~tune_id: id (fun ~dance_id -> Entry.Id.of_string_exn dance_id) in
  Tune_sql.Single.get db ~id (sql_to_tune ~id ~extra_names ~composers ~dances)

let create tune =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make_public db `Tune in
  tune_to_sql ~create_or_update: Tune_sql.create db id tune;%lwt
  lwt id

let update id tune =
  Connection.with_ @@ fun db ->
  Entry_new.touch db id;%lwt
  tune_to_sql ~create_or_update: (fun db ~id -> Tune_sql.update db ~id) db id tune

let delete id =
  Connection.with_ @@ fun db ->
  let tune_id = Entry.Id.to_string id in
  ignore <$> Tune_sql.delete_all_extra_names db ~tune_id;%lwt
  ignore <$> Tune_sql.delete_all_composers db ~tune_id;%lwt
  ignore <$> Tune_sql.delete_all_dances db ~tune_id;%lwt
  ignore <$> Tune_sql.delete db ~id: tune_id;%lwt
  Entry_new.delete db id
