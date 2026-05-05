open Nes

module Log = (val Logs.src_log @@ Logs.Src.create "server.database.migrations": Logs.LOG)

module Migrations_sql = Migrations_sql.Sqlgg(Sqlgg_postgresql)

type m026_2026_04_split_user_json_into_fields__user_value = {
  username: string;
  password: string option; [@default None]
  password_reset_token: (string * Datetime.t) option; [@default None] [@key "password-reset-token"]
  remember_me_tokens: Yojson.Safe.t; [@default `Assoc []] [@key "remember-me-token"] [@of_yojson Result.ok]
  role: Yojson.Safe.t; [@default `List [`String "Normal_user"]] [@of_yojson Result.ok]
}
[@@deriving of_yojson]

type m026_2026_04_split_user_json_into_fields__user_meta = {
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving of_yojson]

type m026_2026_04_split_user_json_into_fields__user = {
  value: m026_2026_04_split_user_json_into_fields__user_value;
  meta: m026_2026_04_split_user_json_into_fields__user_meta;
  access: string list;
}
[@@deriving of_yojson]

type m027_2026_04_split_role_json_into_fields__role =
  | Normal_user
  | Maintainer
  | Administrator of {omniscience: bool}
[@@deriving of_yojson]

type m030_2026_05_split_person_json_into_fields__person_value = {
  name: NEString.t;
  user: string option; [@default None]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  composed_tunes_are_public: bool; [@default false]
  published_tunes_are_public: bool; [@default false]
}
[@@deriving of_yojson]

type m030_2026_05_split_person_json_into_fields__person = {
  value: m030_2026_05_split_person_json_into_fields__person_value;
  meta: m026_2026_04_split_user_json_into_fields__user_meta;
  access: string list;
}
[@@deriving of_yojson]

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
  make_custom "m026_2026_04_split_user_yaml_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m026_2026_04_split_user_json_into_fields__add_columns db in
    let%lwt all = Migrations_sql.List.m026_2026_04_split_user_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let user =
          match m026_2026_04_split_user_json_into_fields__user_of_yojson json with
          | Ok user -> user
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise user: %s" msg);
            assert false
        in
        ignore
        <$> Migrations_sql.m026_2026_04_split_user_json_into_fields__update_one
            db
            ~id
            ~username: (Some user.value.username)
            ~password: user.value.password
            ~password_reset_token_hash: (Option.map fst user.value.password_reset_token)
            ~password_reset_token_max_date: (Option.map snd user.value.password_reset_token)
            ~remember_me_tokens: (Some user.value.remember_me_tokens)
            ~role: (Some user.value.role)
            ~created_at: (Some user.meta.created_at)
            ~modified_at: (Some user.meta.modified_at)
      )
      all;%lwt
    (* NOTE: As of April 2026, Sqlgg does not support `ALTER COLUMN` but only
       the MySQL-specific `MODIFY` or `CHANGE COLUMN`. So we put one of those in
       SQL for Sqlgg to infer the right column types, but exec a
       PostgreSQL-compatible one manually here. *)
    ignore @@
      Connection.bypass_exec
        db
        {|
          ALTER TABLE "user"
            ALTER COLUMN "username" SET NOT NULL,
            ALTER COLUMN "role" SET NOT NULL,
            ALTER COLUMN "remember_me_tokens" SET NOT NULL,
            ALTER COLUMN "created_at" SET NOT NULL,
            ALTER COLUMN "modified_at" SET NOT NULL,
            ADD UNIQUE ("username");
        |};
    let%lwt _ = Migrations_sql.m026_2026_04_split_user_json_into_fields__drop_json_column db in
    lwt_unit
  );
  make_custom "m027_2026_04_split_role_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m027_2026_04_split_role_json_into_fields__add_columns db in
    let%lwt all = Migrations_sql.List.m027_2026_04_split_role_json_into_fields__get_all db (fun ~id ~role -> (id, role)) in
    Lwt_list.iter_s
      (fun (id, role) ->
        let (role_new, omniscience) =
          match Result.get_ok @@ m027_2026_04_split_role_json_into_fields__role_of_yojson role with
          | Normal_user -> (0, false)
          | Maintainer -> (1, false)
          | Administrator {omniscience} -> (2, omniscience)
        in
        ignore
        <$> Migrations_sql.m027_2026_04_split_role_json_into_fields__update_one
            db
            ~id
            ~role_new: (some @@ Int64.of_int role_new)
            ~omniscience: (Some omniscience)
      )
      all;%lwt
    (* NOTE: As of April 2026, Sqlgg does not support `ALTER COLUMN` but only
       the MySQL-specific `MODIFY` or `CHANGE COLUMN`. So we put one of those in
       SQL for Sqlgg to infer the right column types, but exec a
       PostgreSQL-compatible one manually here. *)
    ignore (
      Connection.bypass_exec
        db
        {|
          ALTER TABLE "user"
            DROP COLUMN "role",
            ALTER COLUMN "role_new" TYPE SMALLINT,
            ALTER COLUMN "role_new" SET NOT NULL,
            ALTER COLUMN "omniscience" TYPE BOOLEAN,
            ALTER COLUMN "omniscience" SET NOT NULL;
        |}
    );
    ignore (Connection.bypass_exec db {| ALTER TABLE "user" RENAME COLUMN "role_new" TO "role"; |});
    lwt_unit
  );
  make_ddl "m028_2026_04_add_remember_me_tokens_table" Migrations_sql.m028_2026_04_add_remember_me_tokens_table;
  make_ddl "m029_2026_04_drop_remember_me_tokens_column" Migrations_sql.m029_2026_04_drop_remember_me_tokens_column;
  make_custom "m030_2026_05_split_person_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m030_2026_05_split_person_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m030_2026_05_split_person_json_into_fields__add_column_to_user db in
    let%lwt all = Migrations_sql.List.m030_2026_05_split_person_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let person =
          match m030_2026_05_split_person_json_into_fields__person_of_yojson json with
          | Ok person -> person
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise person: %s" msg);
            assert false
        in
        ignore
        <$> Migrations_sql.m030_2026_05_split_person_json_into_fields__update_one
            db
            ~id
            ~name: (some @@ NEString.to_string person.value.name)
            ~scddb_id: (Option.map Int64.of_int person.value.scddb_id)
            ~composed_tunes_are_public: (Some person.value.composed_tunes_are_public)
            ~published_tunes_are_public: (Some person.value.published_tunes_are_public)
            ~created_at: (Some person.meta.created_at)
            ~modified_at: (Some person.meta.modified_at);%lwt
        match person.value.user with
        | None -> lwt_unit
        | Some user -> ignore <$> Migrations_sql.m030_2026_05_split_person_json_into_fields__update_user db ~id: user ~person_id: (Some id)
      )
      all;%lwt
    (* NOTE: As of May 2026, Sqlgg does not support `ALTER COLUMN` but only
       the MySQL-specific `MODIFY` or `CHANGE COLUMN`. So we put one of those in
       SQL for Sqlgg to infer the right column types, but exec a
       PostgreSQL-compatible one manually here. *)
    ignore (
      Connection.bypass_exec
        db
        {|
            ALTER TABLE "person"
              ALTER COLUMN "name" SET NOT NULL,
              ALTER COLUMN "composed_tunes_are_public" SET NOT NULL,
              ALTER COLUMN "published_tunes_are_public" SET NOT NULL,
              ALTER COLUMN "created_at" SET NOT NULL,
              ALTER COLUMN "modified_at" SET NOT NULL,
              DROP COLUMN "json";
          |}
    );
    ignore <$> Migrations_sql.m030_2026_05_split_person_json_into_fields__add_constraint db;%lwt
    lwt_unit
  );
]

exception Migration_failed of string * exn

let apply_migrations db =
  let rec skip_already_applied_migrations = function
    | [] -> lwt_nil
    | migration :: migrations ->
      Log.debug (fun m -> m "Checking whether migration %S has already been applied" migration.name);
      if%lwt Option.is_some <$> Migrations_sql.get_migration db ~name: migration.name then
        skip_already_applied_migrations migrations
      else
        lwt (migration :: migrations)
  in
  let%lwt _ = Migrations_sql.create_table_migrations db in
  Log.debug (fun m -> m "Checking already applied migrations");
  let%lwt migrations = skip_already_applied_migrations migrations in
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
        (
          try%lwt
            migration.apply db
          with
            | exn ->
              Log.err (fun m -> m "Could not apply migration %S:\n%s\n%s" migration.name (Printexc.to_string exn) (Printexc.get_backtrace ()));
              raise (Migration_failed (migration.name, exn))
        );%lwt
        ignore <$> Migrations_sql.register_migration db ~name: migration.name
      )
      migrations

let apply_migrations () = Connection.with_ apply_migrations
