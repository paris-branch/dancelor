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
