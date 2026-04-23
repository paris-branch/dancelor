open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.migrations": Logs.LOG)

module Migrations_sql = Migrations_sql.Sqlgg(Connection.Sqlgg_mariadb_lwt)

type migration = {
  name: string;
  apply: (Connection.t -> unit Lwt.t);
}

let make_custom name apply =
  {name; apply}

let make_ddl name ddl =
  let apply = fun db -> ignore <$> ddl db in
  make_custom name apply

let migrations : migration list = [
  make_ddl "m001_2026_04_add_book_table" Migrations_sql.m001_2026_04_add_book_table;
  make_ddl "m002_2026_04_add_dance_table" Migrations_sql.m002_2026_04_add_dance_table;
  make_ddl "m003_2026_04_add_person_table" Migrations_sql.m003_2026_04_add_person_table;
  make_ddl "m004_2026_04_add_set_table" Migrations_sql.m004_2026_04_add_set_table;
  make_ddl "m005_2026_04_add_source_table" Migrations_sql.m005_2026_04_add_source_table;
  make_ddl "m006_2026_04_add_tune_table" Migrations_sql.m006_2026_04_add_tune_table;
  make_ddl "m007_2026_04_add_user_table" Migrations_sql.m007_2026_04_add_user_table;
  make_ddl "m008_2026_04_add_version_table" Migrations_sql.m008_2026_04_add_version_table;
  make_ddl "m009_2026_04_add_globally_unique_id_table" Migrations_sql.m009_2026_04_add_globally_unique_id_table;
  make_ddl "m010_2026_04_insert_ids_from_book_into_globally_unique_id" Migrations_sql.m010_2026_04_insert_ids_from_book_into_globally_unique_id;
  make_ddl "m011_2026_04_add_fk_book_id_key" Migrations_sql.m011_2026_04_add_fk_book_id_key;
  make_ddl "m012_2026_04_insert_ids_from_dance_into_globally_unique_id" Migrations_sql.m012_2026_04_insert_ids_from_dance_into_globally_unique_id;
  make_ddl "m013_2026_04_add_fk_dance_id_key" Migrations_sql.m013_2026_04_add_fk_dance_id_key;
  make_ddl "m014_2026_04_insert_ids_from_person_into_globally_unique_id" Migrations_sql.m014_2026_04_insert_ids_from_person_into_globally_unique_id;
  make_ddl "m015_2026_04_add_fk_person_id_key" Migrations_sql.m015_2026_04_add_fk_person_id_key;
  make_ddl "m016_2026_04_insert_ids_from_set_into_globally_unique_id" Migrations_sql.m016_2026_04_insert_ids_from_set_into_globally_unique_id;
  make_ddl "m017_2026_04_add_fk_set_id_key" Migrations_sql.m017_2026_04_add_fk_set_id_key;
  make_ddl "m018_2026_04_insert_ids_from_source_into_globally_unique_id" Migrations_sql.m018_2026_04_insert_ids_from_source_into_globally_unique_id;
  make_ddl "m019_2026_04_add_fk_source_id_key" Migrations_sql.m019_2026_04_add_fk_source_id_key;
  make_ddl "m020_2026_04_insert_ids_from_tune_into_globally_unique_id" Migrations_sql.m020_2026_04_insert_ids_from_tune_into_globally_unique_id;
  make_ddl "m021_2026_04_add_fk_tune_id_key" Migrations_sql.m021_2026_04_add_fk_tune_id_key;
  make_ddl "m022_2026_04_insert_ids_from_user_into_globally_unique_id" Migrations_sql.m022_2026_04_insert_ids_from_user_into_globally_unique_id;
  make_ddl "m023_2026_04_add_fk_user_id_key" Migrations_sql.m023_2026_04_add_fk_user_id_key;
  make_ddl "m024_2026_04_insert_ids_from_version_into_globally_unique_id" Migrations_sql.m024_2026_04_insert_ids_from_version_into_globally_unique_id;
  make_ddl "m025_2026_04_add_fk_version_id_key" Migrations_sql.m025_2026_04_add_fk_version_id_key;
]

let apply_migrations db =
  let rec skip_already_applied_migrations db = function
    | [] -> lwt_nil
    | migration :: migrations ->
      Log.debug (fun m -> m "Checking whether migration %S has already been applied" migration.name);
      if%lwt Migrations_sql.Fold.get_migration db (fun ~applied_at: _ _ -> true) ~name: migration.name false then
        skip_already_applied_migrations db migrations
      else
        lwt (migration :: migrations)
  in
  let%lwt _ = Migrations_sql.create_table_migrations db in
  Log.debug (fun m -> m "Checking already applied migrations");
  let%lwt migrations = skip_already_applied_migrations db migrations in
  match migrations with
  | [] ->
    Log.debug (fun m -> m "There are no remaining migrations");
    lwt_unit
  | first_migration :: _ ->
    Log.debug (fun m -> m "First remaining migration: %S" first_migration.name);
    Log.debug (fun m -> m "Applying %d remaining migrations" (List.length migrations));
    Lwt_list.iter_s
      (fun migration ->
        Log.debug (fun m -> m "Applying migration %S" migration.name);
        migration.apply db;%lwt
        ignore <$> Migrations_sql.register_migration db ~name: migration.name
      )
      migrations

let apply_migrations () = Connection.with_ apply_migrations
