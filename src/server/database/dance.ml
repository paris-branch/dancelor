open Nes
open Dancelor_common
open Model_new
open Search_new

module Dance_sql = Dance_sql.Sqlgg(Sqlgg_postgresql)

let two_chords_to_common = function
  | `Dont_know -> Model_builder.Core.Dance.Dont_know
  | `One_chord -> One_chord
  | `Two_chords -> Two_chords

let two_chords_of_common = function
  | Model_builder.Core.Dance.Dont_know -> `Dont_know
  | One_chord -> `One_chord
  | Two_chords -> `Two_chords

let sql_to_row ~id ~name ~kind ~devisers ~disambiguation ~(k : Dance_row.t -> 'w) : 'w =
  k {
    id = Entry.Id.of_string_exn id;
    name;
    kind = Kind_dance.of_string kind;
    devisers;
    disambiguation;
  }

let sql_to_view ~id ~name ~extra_names ~kind ~devisers ~scddb_id ~disambiguation ~date ~(k : Dance_view.t -> 'w) : 'w =
  k {
    id = Entry.Id.of_string_exn id;
    name;
    extra_names;
    kind = Kind_dance.of_string kind;
    devisers;
    scddb_id = Option.map Int64.to_int scddb_id;
    disambiguation;
    date = Option.map (Option.get % PartialDate.from_string) date;
  }

let get_extra_names_for db dance_ids =
  Utils.fold_to_tbl (Dance_sql.Fold.get_extra_names_for ~dance_ids) db (fun k ~dance_id ~extra_name -> k dance_id extra_name)

let get_devisers_for db dance_ids =
  Utils.fold_to_tbl (Dance_sql.Fold.get_devisers_for ~dance_ids) db (fun k ~dance_id -> Person.sql_to_name ~k: (k dance_id))

let get_row id : Dance_row.t option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt devisers = flip Utils.tbl_get id <$> get_devisers_for db (`One_of [id]) in
  Dance_sql.Single.get_row db ~id (sql_to_row ~id ~devisers ~k: Fun.id)

let get_rows ids : (Dance_id.t, Dance_row.t) Utils.tbl Lwt.t =
  let ids = List.map Entry.Id.to_string ids in
  Connection.with_ @@ fun db ->
  let%lwt devisers_for = get_devisers_for db (`One_of ids) in
  Utils.fold_to_tbl (Dance_sql.Fold.get_rows ~ids) db (fun k ~id -> sql_to_row ~id ~devisers: (Utils.tbl_get devisers_for id) ~k: (k @@ Entry.Id.of_string_exn id))

let get_view id : Dance_view.t option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt extra_names = flip Utils.tbl_get id <$> get_extra_names_for db (`One_of [id]) in
  let%lwt devisers = flip Utils.tbl_get id <$> get_devisers_for db (`One_of [id]) in
  Dance_sql.Single.get_view db ~id (sql_to_view ~extra_names ~devisers ~id ~k: Fun.id)

let search query : (Dance_row.t * float) list Lwt.t =
  let {Query.common = {terms}; specific = {Dance_query.deviser}} = query in
  Connection.with_ @@ fun db ->
  let%lwt devisers = get_devisers_for db `All in
  Dance_sql.List.search
    db
    ~terms
    ~deviser: (Utils.list_option_map_to_sql Entry.Id.to_string deviser)
    (fun ~score ~id -> sql_to_row ~id ~devisers: (Utils.tbl_get devisers id) ~k: (Pair.snoc score))

(* Legacy *)

type t = Model_builder.Core.Dance.t
type entry = Model_builder.Core.Dance.entry

let sql_to_dance
    ~id
    ~name
    ~extra_names
    ~kind
    ~two_chords
    ~scddb_id
    ~disambiguation
    ~date
    ~created_at
    ~modified_at
    ~devisers
  =
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.Dance.make
        ~names: (NEList.cons (NEString.of_string_exn name) extra_names)
        ~kind: (Kind_dance.of_string kind)
        ~two_chords: (two_chords_to_common two_chords)
        ~scddb_id: (Option.map Int64.to_int scddb_id)
        ~disambiguation: (Option.map NEString.of_string_exn disambiguation)
        ~date: (Option.map (Option.get % PartialDate.from_string) date)
        ~devisers
        ()
    )

let dance_to_sql ~create_or_update db id dance =
  (* FIXME: transaction, maybe [Connection.with_transaction] *)
  let id = Entry.Id.to_string id in
  ignore
  <$> create_or_update
      db
      ~id
      ~name: (NEString.to_string @@ NEList.hd @@ Model_builder.Core.Dance.names dance)
      ~kind: (Kind_dance.to_string @@ Model_builder.Core.Dance.kind dance)
      ~two_chords: (two_chords_of_common @@ Model_builder.Core.Dance.two_chords dance)
      ~scddb_id: (Option.map Int64.of_int @@ Model_builder.Core.Dance.scddb_id dance)
      ~disambiguation: (Option.map NEString.to_string @@ Model_builder.Core.Dance.disambiguation dance)
      ~date: (Option.map PartialDate.to_string @@ Model_builder.Core.Dance.date dance);%lwt
  ignore <$> Dance_sql.delete_all_extra_names db ~dance_id: id;%lwt
  Lwt_list.iter_s
    (fun extra_name ->
      ignore <$> Dance_sql.add_one_extra_name db ~dance_id: id ~extra_name: (NEString.to_string extra_name)
    )
    (NEList.tl @@ Model_builder.Core.Dance.names dance);%lwt
  ignore <$> Dance_sql.delete_all_devisers db ~dance_id: id;%lwt
  Lwt_list.iteri_s
    (fun index deviser_id ->
      ignore
      <$> Dance_sql.add_one_deviser
          db
          ~dance_id: id
          ~index: (Int64.of_int index)
          ~deviser_id: (Entry.Id.to_string deviser_id)
    )
    (Model_builder.Core.Dance.devisers dance)

let get id : Model_builder.Core.Dance.entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  let%lwt extra_names = Dance_sql.List.get_extra_names db ~dance_id: id (fun ~extra_name -> NEString.of_string_exn extra_name) in
  let%lwt devisers = Dance_sql.List.get_devisers db ~dance_id: id (fun ~deviser_id -> Entry.Id.of_string_exn deviser_id) in
  Dance_sql.Single.get db ~id (sql_to_dance ~id ~extra_names ~devisers)

let create dance =
  Connection.with_ @@ fun db ->
  let%lwt id = Entry_new.make_public db `Dance in
  dance_to_sql ~create_or_update: Dance_sql.create db id dance;%lwt
  lwt id

let update id dance =
  Connection.with_ @@ fun db ->
  Entry_new.touch db id;%lwt
  dance_to_sql ~create_or_update: (fun db ~id -> Dance_sql.update db ~id) db id dance

let delete id =
  Connection.with_ @@ fun db ->
  let dance_id = Entry.Id.to_string id in
  ignore <$> Dance_sql.delete_all_extra_names db ~dance_id;%lwt
  ignore <$> Dance_sql.delete_all_devisers db ~dance_id;%lwt
  ignore <$> Dance_sql.delete db ~id: dance_id;%lwt
  Entry_new.delete db id
