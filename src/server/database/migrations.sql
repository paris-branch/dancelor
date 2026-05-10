-- -------------------------------------------------------------------------- --
-- Administrative stuff to support automated migrations
-- -------------------------------------------------------------------------- --

-- @create_table_migrations
CREATE TABLE IF NOT EXISTS "migrations" (
    "name" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- @get_migration
SELECT "applied_at" FROM "migrations"
WHERE "name" = @name;

-- @register_migration
INSERT INTO "migrations" ("name") VALUES (@name);

-- -------------------------------------------------------------------------- --
-- First migrations: creation of basic id->json tables
-- -------------------------------------------------------------------------- --

-- @m001_2026_04_add_book_table
CREATE TABLE "book" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m002_2026_04_add_dance_table
CREATE TABLE "dance" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m003_2026_04_add_person_table
CREATE TABLE "person" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m004_2026_04_add_set_table
CREATE TABLE "set" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m005_2026_04_add_source_table
CREATE TABLE "source" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL,
    "cover" BYTEA
);

-- @m006_2026_04_add_tune_table
CREATE TABLE "tune" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m007_2026_04_add_user_table
CREATE TABLE "user" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m008_2026_04_add_version_table
CREATE TABLE "version" (
    "id" VARCHAR(14) PRIMARY KEY,
    "json" JSON NOT NULL
);

-- @m009_2026_04_add_globally_unique_id_table
CREATE TABLE "globally_unique_id" (
    "id" VARCHAR(14) PRIMARY KEY,
    "type" TEXT NOT NULL
);

-- @m010_2026_04_insert_ids_from_book_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Book' AS "type"
FROM "book";

-- @m011_2026_04_add_fk_book_id_key
ALTER TABLE "book"
ADD CONSTRAINT "fk_book_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m012_2026_04_insert_ids_from_dance_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Dance' AS "type"
FROM "dance";

-- @m013_2026_04_add_fk_dance_id_key
ALTER TABLE "dance"
ADD CONSTRAINT "fk_dance_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m014_2026_04_insert_ids_from_person_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Person' AS "type"
FROM "person";

-- @m015_2026_04_add_fk_person_id_key
ALTER TABLE "person"
ADD CONSTRAINT "fk_person_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m016_2026_04_insert_ids_from_set_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Set' AS "type"
FROM "set";

-- @m017_2026_04_add_fk_set_id_key
ALTER TABLE "set"
ADD CONSTRAINT "fk_set_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m018_2026_04_insert_ids_from_source_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Source' AS "type"
FROM "source";

-- @m019_2026_04_add_fk_source_id_key
ALTER TABLE "source"
ADD CONSTRAINT "fk_source_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m020_2026_04_insert_ids_from_tune_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Tune' AS "type"
FROM "tune";

-- @m021_2026_04_add_fk_tune_id_key
ALTER TABLE "tune"
ADD CONSTRAINT "fk_tune_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m022_2026_04_insert_ids_from_user_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'User' AS "type"
FROM "user";

-- @m023_2026_04_add_fk_user_id_key
ALTER TABLE "user"
ADD CONSTRAINT "fk_user_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m024_2026_04_insert_ids_from_version_into_globally_unique_id
INSERT INTO "globally_unique_id"
SELECT
    "id",
    'Version' AS "type"
FROM "version";

-- @m025_2026_04_add_fk_version_id_key
ALTER TABLE "version"
ADD CONSTRAINT "fk_version_id"
FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id");

-- @m026_2026_04_split_user_json_into_fields__add_columns
ALTER TABLE "user"
ADD COLUMN "username" VARCHAR(256),
ADD COLUMN "password" VARCHAR(256),
ADD COLUMN "password_reset_token_hash" VARCHAR(256),
ADD COLUMN "password_reset_token_max_date" TIMESTAMP,
ADD COLUMN "role" JSON,
ADD COLUMN "remember_me_tokens" JSON,
ADD COLUMN "created_at" TIMESTAMP,
ADD COLUMN "modified_at" TIMESTAMP;

-- @m026_2026_04_split_user_json_into_fields__get_all
SELECT
    "id",
    "json"
FROM "user";

-- @m026_2026_04_split_user_json_into_fields__update_one
UPDATE "user"
SET
    "username" = @username,
    "password" = @password,
    "password_reset_token_hash" = @password_reset_token_hash,
    "password_reset_token_max_date" = @password_reset_token_max_date,
    "remember_me_tokens" = @remember_me_tokens,
    "role" = @role,
    "created_at" = @created_at,
    "modified_at" = @modified_at
WHERE "id" = @id;

-- @m026_2026_04_split_user_json_into_fields__set_not_null__for_sqlgg
ALTER TABLE "user"
CHANGE COLUMN "username" "username" VARCHAR(256) NOT NULL UNIQUE,
CHANGE COLUMN "role" "role" JSON NOT NULL,
CHANGE COLUMN "remember_me_tokens" "remember_me_tokens" JSON NOT NULL,
CHANGE COLUMN "created_at" "created_at" TIMESTAMP NOT NULL,
CHANGE COLUMN "modified_at" "modified_at" TIMESTAMP NOT NULL;

-- @m026_2026_04_split_user_json_into_fields__drop_json_column
ALTER TABLE "user"
DROP COLUMN "json";

-- @m027_2026_04_split_role_json_into_fields__add_columns
ALTER TABLE "user"
ADD COLUMN "role_new" SMALLINT,
ADD COLUMN "omniscience" BOOLEAN;

-- @m027_2026_04_split_role_json_into_fields__get_all
SELECT
    "id",
    "role"
FROM "user";

-- @m027_2026_04_split_role_json_into_fields__update_one
UPDATE "user"
SET
    "role_new" = @role_new,
    "omniscience" = @omniscience
WHERE id = @id;

-- @m027_2026_04_split_role_json_into_fields__cleanup_columns__for_sqlgg
ALTER TABLE "user"
DROP COLUMN "role",
CHANGE COLUMN "role_new" "role_new" SMALLINT NOT NULL,
CHANGE COLUMN "omniscience" "omniscience" BOOLEAN NOT NULL,
RENAME COLUMN "role_new" TO "role";

-- @m028_2026_04_add_remember_me_tokens_table
CREATE TABLE "remember_me_tokens" (
    "user_id" VARCHAR(14) NOT NULL,
    "key" VARCHAR(256) NOT NULL,
    "hash" VARCHAR(256) NOT NULL,
    "max_date" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "user" ("id")
);

-- @m029_2026_04_drop_remember_me_tokens_column
ALTER TABLE "user"
DROP COLUMN "remember_me_tokens";

-- @m030_2026_05_split_person_json_into_fields__add_columns
ALTER TABLE "person"
ADD COLUMN "name" VARCHAR(256),
ADD COLUMN "scddb_id" INT,
ADD COLUMN "composed_tunes_are_public" BOOLEAN,
ADD COLUMN "published_tunes_are_public" BOOLEAN,
ADD COLUMN "created_at" TIMESTAMP,
ADD COLUMN "modified_at" TIMESTAMP;

-- @m030_2026_05_split_person_json_into_fields__add_column_to_user
ALTER TABLE "user"
ADD COLUMN "person_id" VARCHAR(14);

-- @m030_2026_05_split_person_json_into_fields__get_all
SELECT
    "id",
    "json"
FROM "person";

-- @m030_2026_05_split_person_json_into_fields__update_one
UPDATE "person"
SET
    "name" = @name,
    "scddb_id" = @scddb_id,
    "composed_tunes_are_public" = @composed_tunes_are_public,
    "published_tunes_are_public" = @published_tunes_are_public,
    "created_at" = @created_at,
    "modified_at" = @modified_at
WHERE "id" = @id;

-- @m030_2026_05_split_person_json_into_fields__update_user
UPDATE "user"
SET
    "person_id" = @person_id
WHERE "id" = @id;

-- @m030_2026_05_split_person_json_into_fields__cleanup_columns__for_sqlgg
ALTER TABLE "person"
CHANGE COLUMN "name" "name" VARCHAR(256) NOT NULL,
CHANGE COLUMN "composed_tunes_are_public" "composed_tunes_are_public" BOOLEAN NOT NULL,
CHANGE COLUMN "published_tunes_are_public" "published_tunes_are_public" BOOLEAN NOT NULL,
CHANGE COLUMN "created_at" "created_at" TIMESTAMP NOT NULL,
CHANGE COLUMN "modified_at" "modified_at" TIMESTAMP NOT NULL,
DROP COLUMN "json";

-- @m030_2026_05_split_person_json_into_fields__add_constraint
ALTER TABLE "user"
ADD CONSTRAINT "fk_user_person_id" FOREIGN KEY ("person_id") REFERENCES "person" ("id");

-- @m031_2026_05_split_source_json_into_fields__add_columns
ALTER TABLE "source"
ADD COLUMN "name" VARCHAR(256),
ADD COLUMN "short_name" VARCHAR(64),
ADD COLUMN "scddb_id" INT,
ADD COLUMN "description" TEXT,
ADD COLUMN "date" VARCHAR(32),
ADD COLUMN "created_at" TIMESTAMP,
ADD COLUMN "modified_at" TIMESTAMP;

-- @m031_2026_05_split_source_json_into_fields__add_source_editors_table
CREATE TABLE "source_editors" (
    "source_id" VARCHAR(14) NOT NULL,
    "person_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_source_editors_source_id" FOREIGN KEY ("source_id") REFERENCES "source" ("id"),
    CONSTRAINT "fk_source_editors_person_id" FOREIGN KEY ("person_id") REFERENCES "person" ("id")
);

-- @m031_2026_05_split_source_json_into_fields__get_all
SELECT
    "id",
    "json"
FROM "source";

-- @m031_2026_05_split_source_json_into_fields__update_one
UPDATE "source"
SET
    "name" = @name,
    "short_name" = @short_name,
    "scddb_id" = @scddb_id,
    "description" = @description,
    "date" = @date,
    "created_at" = @created_at,
    "modified_at" = @modified_at
WHERE "id" = @id;

-- @m031_2026_05_split_source_json_into_fields__add_one_editor
INSERT INTO "source_editors" (
    "source_id",
    "person_id"
) VALUES (
    @source_id,
    @person_id
);

-- @m031_2026_05_split_source_json_into_fields__cleanup_columns__for_sqlgg
ALTER TABLE "source"
CHANGE COLUMN "name" "name" VARCHAR(256) NOT NULL,
CHANGE COLUMN "created_at" "created_at" TIMESTAMP NOT NULL,
CHANGE COLUMN "modified_at" "modified_at" TIMESTAMP NOT NULL,
DROP COLUMN "json";

-- @m032_2026_05_split_dance_json_into_fields__add_columns
ALTER TABLE "dance"
ADD COLUMN "name" VARCHAR(256),
ADD COLUMN "kind" VARCHAR(32),
ADD COLUMN "two_chords" SMALLINT,
ADD COLUMN "scddb_id" INT,
ADD COLUMN "disambiguation" VARCHAR(256),
ADD COLUMN "date" VARCHAR(32),
ADD COLUMN "created_at" TIMESTAMP,
ADD COLUMN "modified_at" TIMESTAMP;

-- @m032_2026_05_split_dance_json_into_fields__add_dance_extra_names_table
CREATE TABLE "dance_extra_names" (
    "dance_id" VARCHAR(14) NOT NULL,
    "extra_name" VARCHAR(256) NOT NULL,
    CONSTRAINT "fk_dance_extra_names_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id")
);

-- @m032_2026_05_split_dance_json_into_fields__add_dance_devisers_table
CREATE TABLE "dance_devisers" (
    "dance_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "deviser_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_dance_devisers_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_dance_devisers_deviser_id" FOREIGN KEY ("deviser_id") REFERENCES "person" ("id")
);

-- @m032_2026_05_split_dance_json_into_fields__get_all
SELECT
    "id",
    "json"
FROM "dance";

-- @m032_2026_05_split_dance_json_into_fields__update_one
UPDATE "dance"
SET
    "name" = @name,
    "kind" = @kind,
    "two_chords" = @two_chords,
    "scddb_id" = @scddb_id,
    "disambiguation" = @disambiguation,
    "date" = @date,
    "created_at" = @created_at,
    "modified_at" = @modified_at
WHERE "id" = @id;

-- @m032_2026_05_split_dance_json_into_fields__add_one_extra_name
INSERT INTO "dance_extra_names" (
    "dance_id",
    "extra_name"
) VALUES (
    @dance_id,
    @extra_name
);

-- @m032_2026_05_split_dance_json_into_fields__add_one_deviser
INSERT INTO "dance_devisers" (
    "dance_id",
    "index",
    "deviser_id"
) VALUES (
    @dance_id,
    @index,
    @deviser_id
);

-- @m032_2026_05_split_dance_json_into_fields__cleanup_columns__for_sqlgg
ALTER TABLE "dance"
CHANGE COLUMN "name" "name" VARCHAR(256) NOT NULL,
CHANGE COLUMN "kind" "kind" VARCHAR(32) NOT NULL,
CHANGE COLUMN "two_chords" "two_chords" SMALLINT NOT NULL,
CHANGE COLUMN "disambiguation" "disambiguation" VARCHAR(256) NOT NULL,
CHANGE COLUMN "created_at" "created_at" TIMESTAMP NOT NULL,
CHANGE COLUMN "modified_at" "modified_at" TIMESTAMP NOT NULL,
DROP COLUMN "json";

-- @m033_2026_05_split_tune_json_into_fields__add_columns
ALTER TABLE "tune"
ADD COLUMN "name" VARCHAR,
ADD COLUMN "kind" VARCHAR(32),
ADD COLUMN "remark" VARCHAR,
ADD COLUMN "scddb_id" INT,
ADD COLUMN "date" VARCHAR(32),
ADD COLUMN "created_at" TIMESTAMP,
ADD COLUMN "modified_at" TIMESTAMP;

-- @m033_2026_05_split_tune_json_into_fields__add_tune_extra_names_table
CREATE TABLE "tune_extra_names" (
    "tune_id" VARCHAR(14) NOT NULL,
    "extra_name" VARCHAR NOT NULL,
    CONSTRAINT "fk_tune_extra_names_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id")
);

-- @m033_2026_05_split_tune_json_into_fields__add_tune_composers_table
CREATE TABLE "tune_composers" (
    "tune_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "composer_id" VARCHAR(14) NOT NULL,
    "details" VARCHAR NOT NULL,
    CONSTRAINT "fk_tune_composers_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id"),
    CONSTRAINT "fk_tune_composers_composer_id" FOREIGN KEY ("composer_id") REFERENCES "person" ("id")
);

-- @m033_2026_05_split_tune_json_into_fields__add_recommended_tunes_table
CREATE TABLE "recommended_tunes" (
    "dance_id" VARCHAR(14) NOT NULL,
    "tune_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_recommended_tunes_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_recommended_tunes_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id")
);

-- @m033_2026_05_split_tune_json_into_fields__get_all
SELECT
    "id",
    "json"
FROM "tune";

-- @m033_2026_05_split_tune_json_into_fields__update_one
UPDATE "tune"
SET
    "name" = @name,
    "kind" = @kind,
    "remark" = @remark,
    "scddb_id" = @scddb_id,
    "date" = @date,
    "created_at" = @created_at,
    "modified_at" = @modified_at
WHERE "id" = @id;

-- @m033_2026_05_split_tune_json_into_fields__add_one_extra_name
INSERT INTO "tune_extra_names" (
    "tune_id",
    "extra_name"
) VALUES (
    @tune_id,
    @extra_name
);

-- @m033_2026_05_split_tune_json_into_fields__add_one_composer
INSERT INTO "tune_composers" (
    "tune_id",
    "index",
    "composer_id",
    "details"
) VALUES (
    @tune_id,
    @index,
    @composer_id,
    @details
);

-- @m033_2026_05_split_tune_json_into_fields__add_one_recommended_tune
INSERT INTO "recommended_tunes" (
    "dance_id",
    "tune_id"
) VALUES (
    @dance_id,
    @tune_id
);

-- @m033_2026_05_split_tune_json_into_fields__cleanup_columns__for_sqlgg
ALTER TABLE "tune"
CHANGE COLUMN "name" "name" VARCHAR NOT NULL,
CHANGE COLUMN "kind" "kind" VARCHAR(32) NOT NULL,
CHANGE COLUMN "remark" "remark" VARCHAR NOT NULL,
CHANGE COLUMN "created_at" "created_at" TIMESTAMP NOT NULL,
CHANGE COLUMN "modified_at" "modified_at" TIMESTAMP NOT NULL,
DROP COLUMN "json";
