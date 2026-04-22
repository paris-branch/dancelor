open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.migrations": Logs.LOG)

module Migrations_sql = Migrations_sql.Sqlgg(Sqlgg_mariadb_lwt)

type migration = {
  name: string;
  apply: (Sqlgg_mariadb_lwt.Mariadb.t -> unit Lwt.t);
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
