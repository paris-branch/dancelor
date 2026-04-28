CREATE TABLE "globally_unique_id" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "type" TEXT NOT NULL
);

CREATE TABLE "book" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_book_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "dance" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_dance_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "person" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_person_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "set" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_set_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "source" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    "cover" BYTEA DEFAULT NULL,
    CONSTRAINT "fk_source_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "tune" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_tune_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "user" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "username" VARCHAR(256) NOT NULL UNIQUE,
    "password" VARCHAR(256),
    "password_reset_token_hash" VARCHAR(256),
    "password_reset_token_max_date" TIMESTAMP,
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    "role" SMALLINT NOT NULL,
    "omniscience" BOOLEAN NOT NULL,
    CONSTRAINT "fk_user_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "remember_me_tokens" (
    "user_id" VARCHAR(14) NOT NULL,
    "key" VARCHAR(256) NOT NULL,
    "hash" VARCHAR(256) NOT NULL,
    "max_date" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "user" ("id")
);

CREATE TABLE "version" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_version_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);
