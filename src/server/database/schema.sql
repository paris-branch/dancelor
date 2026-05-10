CREATE TABLE "globally_unique_id" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "type" TEXT NOT NULL
);

CREATE TABLE "person" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "name" VARCHAR(256) NOT NULL,
    "scddb_id" INT,
    "composed_tunes_are_public" BOOLEAN NOT NULL,
    "published_tunes_are_public" BOOLEAN NOT NULL,
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_person_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "book" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_book_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "dance" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "name" VARCHAR(256) NOT NULL,
    "kind" VARCHAR(32) NOT NULL,
    "two_chords" SMALLINT NOT NULL,
    "scddb_id" INT,
    "disambiguation" VARCHAR(256) NOT NULL,
    "date" VARCHAR(32),
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_dance_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "dance_devisers" (
    "dance_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "deviser_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_dance_devisers_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_dance_devisers_deviser_id" FOREIGN KEY ("deviser_id") REFERENCES "person" ("id")
);

CREATE TABLE "dance_extra_names" (
    "dance_id" VARCHAR(14) NOT NULL,
    "extra_name" VARCHAR(256) NOT NULL,
    CONSTRAINT "fk_dance_extra_names_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id")
);

CREATE TABLE "set" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "json" JSON NOT NULL,
    CONSTRAINT "fk_set_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "source" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "cover" BYTEA DEFAULT NULL,
    "name" VARCHAR(256) NOT NULL,
    "short_name" VARCHAR(64),
    "scddb_id" INT,
    "description" TEXT,
    "date" VARCHAR(32),
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_source_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "source_editors" (
    "source_id" VARCHAR(14) NOT NULL,
    "person_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_source_editors_source_id" FOREIGN KEY ("source_id") REFERENCES "source" ("id"),
    CONSTRAINT "fk_source_editors_person_id" FOREIGN KEY ("person_id") REFERENCES "person" ("id")
);

CREATE TABLE "tune" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "name" VARCHAR NOT NULL,
    "kind" VARCHAR(32) NOT NULL,
    "remark" VARCHAR NOT NULL,
    "scddb_id" INT,
    "date" VARCHAR(32),
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_tune_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "tune_extra_names" (
    "tune_id" VARCHAR(14) NOT NULL,
    "extra_name" VARCHAR NOT NULL,
    CONSTRAINT "fk_tune_extra_names_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id")
);

CREATE TABLE "tune_composers" (
    "tune_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "composer_id" VARCHAR(14) NOT NULL,
    "details" VARCHAR NOT NULL,
    CONSTRAINT "fk_tune_composers_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id"),
    CONSTRAINT "fk_tune_composers_composer_id" FOREIGN KEY ("composer_id") REFERENCES "person" ("id")
);

CREATE TABLE "recommended_tunes" (
    "dance_id" VARCHAR(14) NOT NULL,
    "tune_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_recommended_tunes_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_recommended_tunes_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id")
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
    "person_id" VARCHAR(14) NULL,
    CONSTRAINT "fk_user_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id"),
    CONSTRAINT "fk_user_person_id" FOREIGN KEY ("person_id") REFERENCES "person" ("id")
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
