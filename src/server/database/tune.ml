open Nes
open Dancelor_common
open Model_new
open Search_new

module Tune_sql = Tune_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.Tune.t
type entry = Model_builder.Core.Tune.entry

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

let search query : (Tune_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = {Tune_query.kind; composer}} = query in
  Connection.with_ @@ fun db ->
  let%lwt composers = Utils.fold_to_tbl Tune_sql.Fold.get_all_composers_new db (fun k ~tune_id -> Person.sql_to_name ~k: (k tune_id)) in
  Tune_sql.List.search
    db
    ~terms
    ~kind: (Option.map (List.map kind_base_to_sql) kind)
    ~composer: (Utils.list_option_map_to_sql Entry.Id.to_string composer)
    (fun ~score ~id -> sql_to_row ~id ~composers: (Utils.tbl_get composers id) ~k: (Pair.snoc score))

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

let get_all () =
  Connection.with_ @@ fun db ->
  let extra_names = Hashtbl.create 8 in
  let composers = Hashtbl.create 8 in
  let dances = Hashtbl.create 8 in
  Tune_sql.Fold.get_all_extra_names
    db
    (fun ~tune_id ~extra_name () ->
      Hashtbl.add extra_names tune_id (NEString.of_string_exn extra_name)
    )
    ();%lwt
  Tune_sql.Fold.get_all_composers
    db
    (fun ~tune_id ~composer_id ~details () ->
      Hashtbl.add composers tune_id (Entry.Id.of_string_exn composer_id, Option.map NEString.of_string_exn details)
    )
    ();%lwt
  Tune_sql.Fold.get_all_dances
    db
    (fun ~tune_id ~dance_id () ->
      Hashtbl.add dances tune_id (Entry.Id.of_string_exn dance_id)
    )
    ();%lwt
  Tune_sql.List.get_all db (fun ~id ->
    sql_to_tune
      ~id
      ~extra_names: (List.rev @@ Hashtbl.find_all extra_names id)
      ~composers: (List.rev @@ Hashtbl.find_all composers id)
      ~dances: (List.rev @@ Hashtbl.find_all dances id)
  )

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
