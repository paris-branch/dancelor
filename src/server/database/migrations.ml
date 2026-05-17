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

type m026_m036_2026_04_05_split_jsons_into_fields__meta = {
  created_at: Datetime.t; [@key "created-at"]
  modified_at: Datetime.t; [@key "modified-at"]
}
[@@deriving of_yojson]

type m026_2026_04_split_user_json_into_fields__user = {
  value: m026_2026_04_split_user_json_into_fields__user_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
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
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: string list;
}
[@@deriving of_yojson]

type m031_2026_05_split_source_json_into_fields__source_value = {
  name: NEString.t;
  short_name: NEString.t option; [@default None] [@key "short-name"]
  editors: string list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  description: string option; [@default None]
  date: string option; [@default None]
}
[@@deriving of_yojson]

type m031_2026_05_split_source_json_into_fields__source = {
  value: m031_2026_05_split_source_json_into_fields__source_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: string list;
}
[@@deriving of_yojson]

type m032_2026_05_split_dance_json_into_fields__dance_value_two_chords =
  | Dont_know
  | One_chord
  | Two_chords
[@@deriving of_yojson]

type m032_2026_05_split_dance_json_into_fields__dance_value = {
  names_: NEString.t NEList.t; [@key "names"]
  kind: string;
  devisers: string list; [@default []]
  two_chords: m032_2026_05_split_dance_json_into_fields__dance_value_two_chords; [@default Dont_know] [@key "two-chords"]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  disambiguation: string; [@default ""]
  date: string option; [@default None]
}
[@@deriving of_yojson]

type m032_2026_05_split_dance_json_into_fields__dance = {
  value: m032_2026_05_split_dance_json_into_fields__dance_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: string list;
}
[@@deriving of_yojson]

type m033_2026_05_split_tune_json_into_fields__tune_value_composer = {
  composer: string;
  details: string; [@default ""]
}
[@@deriving of_yojson]

type m033_2026_05_split_tune_json_into_fields__tune_value = {
  names_: NEString.t NEList.t; [@key "names"]
  kind: string;
  composers: m033_2026_05_split_tune_json_into_fields__tune_value_composer list; [@default []]
  dances: string list; [@default []]
  remark: string; [@default ""]
  scddb_id: int option; [@default None] [@key "scddb-id"]
  date: string option; [@default None]
}
[@@deriving of_yojson]

type m033_2026_05_split_tune_json_into_fields__tune = {
  value: m033_2026_05_split_tune_json_into_fields__tune_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: string list;
}
[@@deriving of_yojson]

let m034_2026_05_split_version_json_into_fields__part_int_to_string p =
  String.make 1 (Char.chr (p + 65))

type m034_2026_05_split_version_json_into_fields__version_value_source = {
  source: string;
  structure: string;
  details: string; [@default ""]
}
[@@deriving of_yojson]

type m034_2026_05_split_version_json_into_fields__version_value_content_destructured_voices = {
  melody: string;
  chords: string;
}
[@@deriving of_yojson]

type m034_2026_05_split_version_json_into_fields__version_value_content_destructured = {
  parts: m034_2026_05_split_version_json_into_fields__version_value_content_destructured_voices list;
  transitions: (string * string * m034_2026_05_split_version_json_into_fields__version_value_content_destructured_voices) list;
  default_structure: string;
}
[@@deriving of_yojson]

type m034_2026_05_split_version_json_into_fields__version_value_content =
  | No_content
  | Destructured of m034_2026_05_split_version_json_into_fields__version_value_content_destructured
  | Monolithic of {lilypond: string; bars: int; structure: string}
[@@deriving of_yojson]

type m034_2026_05_split_version_json_into_fields__version_value = {
  tune: string;
  key: string;
  sources: m034_2026_05_split_version_json_into_fields__version_value_source list; [@default []]
  arrangers: string list; [@default []]
  remark: string; [@default ""]
  disambiguation: string; [@default ""]
  content: m034_2026_05_split_version_json_into_fields__version_value_content;
}
[@@deriving of_yojson]

type m034_2026_05_split_version_json_into_fields__version = {
  value: m034_2026_05_split_version_json_into_fields__version_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: string list;
}
[@@deriving of_yojson]

type m035_m036_2026_05_split_jsons_into_fields__version_parameters = {
  transposition: int option; [@default None]
  first_bar: int option; [@default None] [@key "first-bar"]
  clef: string option; [@default None]
  structure: string option; [@default None]
  trivia: string option; [@default None]
  display_name: string option; [@default None] [@key "display-name"]
  display_composer: string option [@default None] [@key "display-composer"]
}
[@@deriving of_yojson]

type m035_2026_05_split_set_json_into_fields__set_value = {
  name: string;
  conceptors: string list; [@default []]
  kind: string;
  contents: (string * m035_m036_2026_05_split_jsons_into_fields__version_parameters) list; [@key "versions-and-parameters"] [@default []]
  order: string;
  instructions: string; [@default ""]
  dances: string list; [@default []]
  remark: string; [@default ""]
}
[@@deriving of_yojson]

type m035_2026_05_split_set_json_into_fields__access_private_visibility =
  | Owners_only
  | Everyone
  | Select_viewers of string list
[@@deriving of_yojson]

type m035_m036_2026_05_split_jsons_into_fields__access_private = {
  owners: string list;
  visibility: m035_2026_05_split_set_json_into_fields__access_private_visibility; [@default Owners_only]
}
[@@deriving of_yojson]

type m035_2026_05_split_set_json_into_fields__set = {
  value: m035_2026_05_split_set_json_into_fields__set_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: m035_m036_2026_05_split_jsons_into_fields__access_private;
}
[@@deriving of_yojson]

type m036_2026_05_split_book_json_into_fields__set_parameters = {
  display_name: string option; [@default None] [@key "display-name"]
  display_conceptor: string option; [@default None] [@key "display-conceptor"]
  display_kind: string option; [@default None] [@key "display-kind"]
  every_version: m035_m036_2026_05_split_jsons_into_fields__version_parameters; [@default (Result.get_ok (m035_m036_2026_05_split_jsons_into_fields__version_parameters_of_yojson (`Assoc [])))] [@key "every-version"]
}
[@@deriving of_yojson]

type m036_2026_05_split_book_json_into_fields__book_value_page_dance =
  | Dance_only
  | Dance_versions of (string * m035_m036_2026_05_split_jsons_into_fields__version_parameters) list
  | Dance_set of string * m036_2026_05_split_book_json_into_fields__set_parameters
[@@deriving of_yojson]

type m036_2026_05_split_book_json_into_fields__book_value_page =
  | Part of string
  | Dance of string * m036_2026_05_split_book_json_into_fields__book_value_page_dance
  | Versions of (string * m035_m036_2026_05_split_jsons_into_fields__version_parameters) list
  | Set of string * m036_2026_05_split_book_json_into_fields__set_parameters
[@@deriving of_yojson]

type m036_2026_05_split_book_json_into_fields__book_value = {
  title: string;
  authors: string list; [@default []]
  date: string option; [@default None]
  contents: m036_2026_05_split_book_json_into_fields__book_value_page list;
  remark: string; [@default ""]
  sources: string list; [@default []]
  scddb_id: int option; [@default None] [@key "scddb-id"]
}
[@@deriving of_yojson]

type m036_2026_05_split_book_json_into_fields__book = {
  value: m036_2026_05_split_book_json_into_fields__book_value;
  meta: m026_m036_2026_04_05_split_jsons_into_fields__meta;
  access: m035_m036_2026_05_split_jsons_into_fields__access_private;
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
  make_custom "m031_2026_05_split_source_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m031_2026_05_split_source_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m031_2026_05_split_source_json_into_fields__add_source_editors_table db in
    let%lwt all = Migrations_sql.List.m031_2026_05_split_source_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let source =
          match m031_2026_05_split_source_json_into_fields__source_of_yojson json with
          | Ok source -> source
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        ignore
        <$> Migrations_sql.m031_2026_05_split_source_json_into_fields__update_one
            db
            ~id
            ~name: (some @@ NEString.to_string source.value.name)
            ~short_name: (Option.map NEString.to_string source.value.short_name)
            ~scddb_id: (Option.map Int64.of_int source.value.scddb_id)
            ~description: source.value.description
            ~date: source.value.date
            ~created_at: (Some source.meta.created_at)
            ~modified_at: (Some source.meta.modified_at);%lwt
        Lwt_list.iter_s
          (fun person_id ->
            ignore
            <$> Migrations_sql.m031_2026_05_split_source_json_into_fields__add_one_editor
                db
                ~source_id: id
                ~person_id
          )
          source.value.editors
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
            ALTER TABLE "source"
              ALTER COLUMN "name" SET NOT NULL,
              ALTER COLUMN "created_at" SET NOT NULL,
              ALTER COLUMN "modified_at" SET NOT NULL,
              DROP COLUMN "json";
          |}
    );
    lwt_unit
  );
  make_custom "m032_2026_05_split_dance_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m032_2026_05_split_dance_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m032_2026_05_split_dance_json_into_fields__add_dance_extra_names_table db in
    let%lwt _ = Migrations_sql.m032_2026_05_split_dance_json_into_fields__add_dance_devisers_table db in
    let%lwt all = Migrations_sql.List.m032_2026_05_split_dance_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let dance =
          match m032_2026_05_split_dance_json_into_fields__dance_of_yojson json with
          | Ok dance -> dance
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        let two_chords =
          match dance.value.two_chords with
          | Dont_know -> 0
          | One_chord -> 1
          | Two_chords -> 2
        in
        ignore
        <$> Migrations_sql.m032_2026_05_split_dance_json_into_fields__update_one
            db
            ~id
            ~name: (some @@ NEString.to_string @@ NEList.hd dance.value.names_)
            ~kind: (Some dance.value.kind)
            ~two_chords: (some @@ Int64.of_int two_chords)
            ~scddb_id: (Option.map Int64.of_int dance.value.scddb_id)
            ~disambiguation: (Some dance.value.disambiguation)
            ~date: dance.value.date
            ~created_at: (Some dance.meta.created_at)
            ~modified_at: (Some dance.meta.modified_at);%lwt
        Lwt_list.iter_s
          (fun extra_name ->
            ignore
            <$> Migrations_sql.m032_2026_05_split_dance_json_into_fields__add_one_extra_name
                db
                ~dance_id: id
                ~extra_name
          )
          (List.map NEString.to_string @@ NEList.tl dance.value.names_);%lwt
        Lwt_list.iteri_s
          (fun index deviser_id ->
            ignore
            <$> Migrations_sql.m032_2026_05_split_dance_json_into_fields__add_one_deviser
                db
                ~dance_id: id
                ~index: (Int64.of_int index)
                ~deviser_id
          )
          dance.value.devisers
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
            ALTER TABLE "dance"
              ALTER COLUMN "name" SET NOT NULL,
              ALTER COLUMN "kind" SET NOT NULL,
              ALTER COLUMN "two_chords" SET NOT NULL,
              ALTER COLUMN "disambiguation" SET NOT NULL,
              ALTER COLUMN "created_at" SET NOT NULL,
              ALTER COLUMN "modified_at" SET NOT NULL,
              DROP COLUMN "json";
          |}
    );
    lwt_unit
  );
  make_custom "m033_2026_05_split_tune_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_tune_extra_names_table db in
    let%lwt _ = Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_tune_composers_table db in
    let%lwt _ = Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_recommended_tunes_table db in
    let%lwt all = Migrations_sql.List.m033_2026_05_split_tune_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let tune =
          match m033_2026_05_split_tune_json_into_fields__tune_of_yojson json with
          | Ok tune -> tune
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        ignore
        <$> Migrations_sql.m033_2026_05_split_tune_json_into_fields__update_one
            db
            ~id
            ~name: (some @@ NEString.to_string @@ NEList.hd tune.value.names_)
            ~kind: (Some tune.value.kind)
            ~remark: (Some tune.value.remark)
            ~scddb_id: (Option.map Int64.of_int tune.value.scddb_id)
            ~date: tune.value.date
            ~created_at: (Some tune.meta.created_at)
            ~modified_at: (Some tune.meta.modified_at);%lwt
        Lwt_list.iter_s
          (fun extra_name ->
            ignore
            <$> Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_one_extra_name
                db
                ~tune_id: id
                ~extra_name
          )
          (List.map NEString.to_string @@ NEList.tl tune.value.names_);%lwt
        Lwt_list.iteri_s
          (fun index {composer; details} ->
            ignore
            <$> Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_one_composer
                db
                ~tune_id: id
                ~index: (Int64.of_int index)
                ~composer_id: composer
                ~details: details
          )
          tune.value.composers;%lwt
        Lwt_list.iter_s
          (fun dance_id ->
            ignore
            <$> Migrations_sql.m033_2026_05_split_tune_json_into_fields__add_one_recommended_tune
                db
                ~tune_id: id
                ~dance_id
          )
          tune.value.dances
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
          ALTER TABLE "tune"
            ALTER COLUMN "name" SET NOT NULL,
            ALTER COLUMN "kind" SET NOT NULL,
            ALTER COLUMN "remark" SET NOT NULL,
            ALTER COLUMN "created_at" SET NOT NULL,
            ALTER COLUMN "modified_at" SET NOT NULL,
            DROP COLUMN "json";
        |}
    );
    lwt_unit
  );
  make_custom "m034_2026_05_split_version_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m034_2026_05_split_version_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m034_2026_05_split_version_json_into_fields__add_version_arrangers_table db in
    let%lwt _ = Migrations_sql.m034_2026_05_split_version_json_into_fields__add_version_sources_table db in
    let%lwt _ = Migrations_sql.m034_2026_05_split_version_json_into_fields__add_version_destructured_parts_table db in
    let%lwt _ = Migrations_sql.m034_2026_05_split_version_json_into_fields__add_version_destructured_transitions_table db in
    let%lwt all = Migrations_sql.List.m034_2026_05_split_version_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let version =
          match m034_2026_05_split_version_json_into_fields__version_of_yojson json with
          | Ok version -> version
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        let (monolithic_lilypond, monolithic_bars, monolithic_or_default_structure) =
          match version.value.content with
          | No_content -> (None, None, None)
          | Destructured {default_structure; _} -> (None, None, Some default_structure)
          | Monolithic {lilypond; bars; structure} -> (Some lilypond, Some (Int64.of_int bars), Some structure)
        in
        ignore
        <$> Migrations_sql.m034_2026_05_split_version_json_into_fields__update_one
            db
            ~id
            ~tune_id: (Some version.value.tune)
            ~key: (Some version.value.key)
            ~remark: (Some version.value.remark)
            ~disambiguation: (Some version.value.disambiguation)
            ~monolithic_lilypond
            ~monolithic_bars
            ~monolithic_or_default_structure
            ~created_at: (Some version.meta.created_at)
            ~modified_at: (Some version.meta.modified_at);%lwt
        Lwt_list.iter_s
          (fun arranger ->
            ignore
            <$> Migrations_sql.m034_2026_05_split_version_json_into_fields__add_one_arranger
                db
                ~version_id: id
                ~arranger_id: arranger
          )
          version.value.arrangers;%lwt
        Lwt_list.iter_s
          (fun source ->
            ignore
            <$> Migrations_sql.m034_2026_05_split_version_json_into_fields__add_one_source
                db
                ~version_id: id
                ~source_id: source.source
                ~structure: source.structure
                ~details: source.details
          )
          version.value.sources;%lwt
        (
          match version.value.content with
          | No_content | Monolithic _ -> lwt_unit
          | Destructured {parts; transitions; default_structure = _} ->
            Lwt_list.iteri_s
              (fun i part ->
                ignore
                <$> Migrations_sql.m034_2026_05_split_version_json_into_fields__add_one_destructured_part
                    db
                    ~version_id: id
                    ~part: (m034_2026_05_split_version_json_into_fields__part_int_to_string i)
                    ~melody: part.melody
                    ~chords: part.chords
              )
              parts;%lwt
            Lwt_list.iter_s
              (fun (from_parts, to_parts, voices) ->
                ignore
                <$> Migrations_sql.m034_2026_05_split_version_json_into_fields__add_one_destructured_transition
                    db
                    ~version_id: id
                    ~from_parts
                    ~to_parts
                    ~melody: voices.melody
                    ~chords: voices.chords
              )
              transitions
        )
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
          ALTER TABLE "version"
            ALTER COLUMN "tune_id" SET NOT NULL,
            ALTER COLUMN "key" SET NOT NULL,
            ALTER COLUMN "remark" SET NOT NULL,
            ALTER COLUMN "disambiguation" SET NOT NULL,
            ALTER COLUMN "created_at" SET NOT NULL,
            ALTER COLUMN "modified_at" SET NOT NULL,
            ADD CONSTRAINT "fk_version_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id"),
            DROP COLUMN "json";
        |}
    );
    lwt_unit
  );
  make_custom "m035_2026_05_split_set_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_conceptors_table db in
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_dances_table db in
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_content_table db in
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_viewers_table db in
    let%lwt _ = Migrations_sql.m035_2026_05_split_set_json_into_fields__add_owners_table db in
    let%lwt all = Migrations_sql.List.m035_2026_05_split_set_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let set =
          match m035_2026_05_split_set_json_into_fields__set_of_yojson json with
          | Ok set -> set
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        let (visibility, viewers) =
          match set.access.visibility with
          | Owners_only -> (0, [])
          | Everyone -> (1, [])
          | Select_viewers viewers -> (2, viewers)
        in
        ignore
        <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__update_one
            db
            ~id
            ~name: (Some set.value.name)
            ~kind: (Some set.value.kind)
            ~order: (Some set.value.order)
            ~instructions: (Some set.value.instructions)
            ~remark: (Some set.value.remark)
            ~created_at: (Some set.meta.created_at)
            ~modified_at: (Some set.meta.modified_at)
            ~visibility: (some @@ Int64.of_int visibility);%lwt
        Lwt_list.iter_s
          (fun conceptor ->
            ignore
            <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__add_one_conceptor
                db
                ~set_id: id
                ~conceptor_id: conceptor
          )
          set.value.conceptors;%lwt
        Lwt_list.iter_s
          (fun dance ->
            ignore
            <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__add_one_dance
                db
                ~set_id: id
                ~dance_id: dance
          )
          set.value.dances;%lwt
        Lwt_list.iteri_s
          (fun index (version, params) ->
            ignore
            <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__add_one_content_item
                db
                ~set_id: id
                ~index: (Int64.of_int index)
                ~version_id: version
                ~version_parameter_transposition_semitones: (Option.map Int64.of_int params.transposition)
                ~version_parameter_first_bar: (Option.map Int64.of_int params.first_bar)
                ~version_parameter_clef: params.clef
                ~version_parameter_structure: params.structure
                ~version_parameter_trivia: params.trivia
                ~version_parameter_display_name: params.display_name
                ~version_parameter_display_composer: params.display_composer
          )
          set.value.contents;%lwt
        Lwt_list.iter_s
          (fun viewer ->
            ignore
            <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__add_one_viewer
                db
                ~set_id: id
                ~viewer_id: viewer
          )
          viewers;%lwt
        Lwt_list.iter_s
          (fun owner ->
            ignore
            <$> Migrations_sql.m035_2026_05_split_set_json_into_fields__add_one_owner
                db
                ~set_id: id
                ~owner_id: owner
          )
          set.access.owners
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
          ALTER TABLE "set"
            ALTER COLUMN "name" SET NOT NULL,
            ALTER COLUMN "kind" SET NOT NULL,
            ALTER COLUMN "order" SET NOT NULL,
            ALTER COLUMN "instructions" SET NOT NULL,
            ALTER COLUMN "remark" SET NOT NULL,
            ALTER COLUMN "created_at" SET NOT NULL,
            ALTER COLUMN "modified_at" SET NOT NULL,
            ALTER COLUMN "visibility" SET NOT NULL,
            DROP COLUMN "json";
        |}
    );
    lwt_unit
  );
  make_custom "m036_2026_05_split_book_json_into_fields" (fun db ->
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_columns db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_authors_table db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_sources_table db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_content_table db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_content_versions_table db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_viewers_table db in
    let%lwt _ = Migrations_sql.m036_2026_05_split_book_json_into_fields__add_owners_table db in
    let%lwt all = Migrations_sql.List.m036_2026_05_split_book_json_into_fields__get_all db (fun ~id ~json -> (id, json)) in
    Lwt_list.iter_s
      (fun (id, json) ->
        let book =
          match m036_2026_05_split_book_json_into_fields__book_of_yojson json with
          | Ok book -> book
          | Error msg ->
            Log.err (fun m -> m "Could not unserialise: %s" msg);
            assert false
        in
        let (visibility, viewers) =
          match book.access.visibility with
          | Owners_only -> (0, [])
          | Everyone -> (1, [])
          | Select_viewers viewers -> (2, viewers)
        in
        ignore
        <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__update_one
            db
            ~id
            ~title: (Some book.value.title)
            ~date: book.value.date
            ~remark: (Some book.value.remark)
            ~scddb_id: (Option.map Int64.of_int book.value.scddb_id)
            ~created_at: (Some book.meta.created_at)
            ~modified_at: (Some book.meta.modified_at)
            ~visibility: (some @@ Int64.of_int visibility);%lwt
        Lwt_list.iter_s
          (fun author ->
            ignore
            <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_author
                db
                ~book_id: id
                ~author_id: author
          )
          book.value.authors;%lwt
        Lwt_list.iter_s
          (fun source ->
            ignore
            <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_source
                db
                ~book_id: id
                ~source_id: source
          )
          book.value.sources;%lwt
        let set_params_none = Result.get_ok @@ m036_2026_05_split_book_json_into_fields__set_parameters_of_yojson @@ `Assoc [] in
        Lwt_list.iteri_s
          (fun content_index page ->
            let (page_type, part_title, dance_id, set_id, set_params, versions_and_params) =
              match page with
              | Part title -> (0L, Some title, None, None, set_params_none, [])
              | Dance (dance, Dance_only) -> (1L, None, Some dance, None, set_params_none, [])
              | Dance (dance, Dance_versions versions_and_params) -> (2L, None, Some dance, None, set_params_none, versions_and_params)
              | Dance (dance, Dance_set (set, set_params)) -> (3L, None, Some dance, Some set, set_params, [])
              | Versions versions_and_params -> (4L, None, None, None, set_params_none, versions_and_params)
              | Set (set, set_params) -> (5L, None, None, Some set, set_params, [])
            in
            let%lwt _ =
              Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_content_item
                db
                ~book_id: id
                ~index: (Int64.of_int content_index)
                ~page_type
                ~part_title
                ~dance_id
                ~set_id
                ~set_parameter_display_name: set_params.display_name
                ~set_parameter_display_conceptor: set_params.display_conceptor
                ~set_parameter_display_kind: set_params.display_kind
                ~set_parameter_version_parameter_transposition_semitones: (Option.map Int64.of_int set_params.every_version.transposition)
                ~set_parameter_version_parameter_first_bar: (Option.map Int64.of_int set_params.every_version.first_bar)
                ~set_parameter_version_parameter_clef: set_params.every_version.clef
                ~set_parameter_version_parameter_structure: set_params.every_version.structure
                ~set_parameter_version_parameter_trivia: set_params.every_version.trivia
                ~set_parameter_version_parameter_display_name: set_params.every_version.display_name
                ~set_parameter_version_parameter_display_composer: set_params.every_version.display_composer
            in
            Lwt_list.iteri_s
              (fun index (version, version_params) ->
                ignore
                <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_content_version
                    db
                    ~book_id: id
                    ~content_index: (Int64.of_int content_index)
                    ~index: (Int64.of_int index)
                    ~version_id: version
                    ~version_parameter_transposition_semitones: (Option.map Int64.of_int version_params.transposition)
                    ~version_parameter_first_bar: (Option.map Int64.of_int version_params.first_bar)
                    ~version_parameter_clef: version_params.clef
                    ~version_parameter_structure: version_params.structure
                    ~version_parameter_trivia: version_params.trivia
                    ~version_parameter_display_name: version_params.display_name
                    ~version_parameter_display_composer: version_params.display_composer
              )
              versions_and_params
          )
          book.value.contents;%lwt
        Lwt_list.iter_s
          (fun viewer ->
            ignore
            <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_viewer
                db
                ~book_id: id
                ~viewer_id: viewer
          )
          viewers;%lwt
        Lwt_list.iter_s
          (fun owner ->
            ignore
            <$> Migrations_sql.m036_2026_05_split_book_json_into_fields__add_one_owner
                db
                ~book_id: id
                ~owner_id: owner
          )
          book.access.owners
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
            ALTER TABLE "book"
              ALTER COLUMN "title" SET NOT NULL,
              ALTER COLUMN "remark" SET NOT NULL,
              ALTER COLUMN "created_at" SET NOT NULL,
              ALTER COLUMN "modified_at" SET NOT NULL,
              ALTER COLUMN "visibility" SET NOT NULL,
              DROP COLUMN "json";
        |}
    );
    lwt_unit
  );
  make_ddl "m037_2026_05_alter_table_set_drop_column_instructions" Migrations_sql.m037_2026_05_alter_table_set_drop_column_instructions;
  make_ddl "m038_2026_05_drop_table_set_dances" Migrations_sql.m038_2026_05_drop_table_set_dances;
  make_ddl "m039_2026_05_alter_table_book_rename_column_title_to_name" Migrations_sql.m039_2026_05_alter_table_book_rename_column_title_to_name;
  make_ddl "m040_2026_05_add_unique_constraint_remember_me_tokens_user_id_key" Migrations_sql.m040_2026_05_add_unique_constraint_remember_me_tokens_user_id_key;
  make_ddl "m041_2026_05_add_unique_constraint_source_editors_source_id_person_id" Migrations_sql.m041_2026_05_add_unique_constraint_source_editors_source_id_person_id;
  make_ddl "m042_2026_05_add_unique_constraints_dance_devisers" Migrations_sql.m042_2026_05_add_unique_constraints_dance_devisers;
  make_ddl "m043_2026_05_add_unique_constraints_tune_composers" Migrations_sql.m043_2026_05_add_unique_constraints_tune_composers;
  make_ddl "m044_2026_05_add_unique_constraint_recommended_tunes_dance_id_tune_id" Migrations_sql.m044_2026_05_add_unique_constraint_recommended_tunes_dance_id_tune_id;
  make_ddl "m045_2026_05_add_unique_constraint_version_arrangers_version_id_arranger_id" Migrations_sql.m045_2026_05_add_unique_constraint_version_arrangers_version_id_arranger_id;
  make_ddl "m046_2026_05_add_unique_constraint_version_sources_version_id_source_id_structure" Migrations_sql.m046_2026_05_add_unique_constraint_version_sources_version_id_source_id_structure;
  make_ddl "m047_2026_05_add_unique_constraint_version_destructured_parts_version_id_part" Migrations_sql.m047_2026_05_add_unique_constraint_version_destructured_parts_version_id_part;
  make_ddl "m048_2026_05_add_unique_constraint_version_destructured_transitions_version_id_from_parts_to_parts" Migrations_sql.m048_2026_05_add_unique_constraint_version_destructured_transitions_version_id_from_parts_to_parts;
  make_ddl "m049_2026_05_add_unique_constraint_set_conceptors_set_id_conceptor_id" Migrations_sql.m049_2026_05_add_unique_constraint_set_conceptors_set_id_conceptor_id;
  make_ddl "m050_2026_05_add_unique_constraint_set_content_set_id_index" Migrations_sql.m050_2026_05_add_unique_constraint_set_content_set_id_index;
  make_ddl "m051_2026_05_add_unique_constraint_set_viewers_set_id_viewer_id" Migrations_sql.m051_2026_05_add_unique_constraint_set_viewers_set_id_viewer_id;
  make_ddl "m052_2026_05_add_unique_constraint_set_owners_set_id_owner_id" Migrations_sql.m052_2026_05_add_unique_constraint_set_owners_set_id_owner_id;
  make_ddl "m053_2026_05_add_unique_constraint_book_authors_book_id_author_id" Migrations_sql.m053_2026_05_add_unique_constraint_book_authors_book_id_author_id;
  make_ddl "m054_2026_05_add_unique_constraint_book_sources_book_id_source_id" Migrations_sql.m054_2026_05_add_unique_constraint_book_sources_book_id_source_id;
  make_ddl "m055_2026_05_add_unique_constraint_book_content_book_id_index" Migrations_sql.m055_2026_05_add_unique_constraint_book_content_book_id_index;
  make_ddl "m056_2026_05_add_unique_constraint_book_content_versions_book_id_content_index_index" Migrations_sql.m056_2026_05_add_unique_constraint_book_content_versions_book_id_content_index_index;
  make_ddl "m057_2026_05_add_unique_constraint_book_viewers_book_id_viewer_id" Migrations_sql.m057_2026_05_add_unique_constraint_book_viewers_book_id_viewer_id;
  make_ddl "m058_2026_05_add_unique_constraint_book_owners_book_id_owner_id" Migrations_sql.m058_2026_05_add_unique_constraint_book_owners_book_id_owner_id;
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
