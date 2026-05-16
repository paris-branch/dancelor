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
    CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "user" ("id"),
    CONSTRAINT "uq_remember_me_tokens_user_id_key" UNIQUE ("user_id", "key")
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
    CONSTRAINT "fk_source_editors_person_id" FOREIGN KEY ("person_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_source_editors_source_id_person_id" UNIQUE ("source_id", "person_id")
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
    CONSTRAINT "fk_dance_devisers_deviser_id" FOREIGN KEY ("deviser_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_dance_devisers_dance_id_deviser_id" UNIQUE ("dance_id", "deviser_id"),
    CONSTRAINT "uq_dance_devisers_dance_id_index_deviser_id" UNIQUE ("dance_id", "index", "deviser_id")
);

CREATE TABLE "dance_extra_names" (
    "dance_id" VARCHAR(14) NOT NULL,
    "extra_name" VARCHAR(256) NOT NULL,
    CONSTRAINT "fk_dance_extra_names_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id")
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
    CONSTRAINT "fk_tune_composers_composer_id" FOREIGN KEY ("composer_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_tune_composers_tune_id_composer_id" UNIQUE ("tune_id", "composer_id"),
    CONSTRAINT "uq_tune_composers_tune_id_index_composer_id" UNIQUE ("tune_id", "index", "composer_id")
);

CREATE TABLE "recommended_tunes" (
    "dance_id" VARCHAR(14) NOT NULL,
    "tune_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_recommended_tunes_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_recommended_tunes_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id"),
    CONSTRAINT "uq_recommended_tunes_dance_id_tune_id" UNIQUE ("dance_id", "tune_id")
);

CREATE TABLE "version" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "tune_id" VARCHAR(14) NOT NULL,
    "key" VARCHAR(32) NOT NULL,
    "remark" VARCHAR NOT NULL,
    "disambiguation" VARCHAR NOT NULL,
    "monolithic_lilypond" TEXT,
    "monolithic_bars" INT,
    "monolithic_or_default_structure" VARCHAR(32),
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    CONSTRAINT "fk_version_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id"),
    CONSTRAINT "fk_version_tune_id" FOREIGN KEY ("tune_id") REFERENCES "tune" ("id")
);

CREATE TABLE "version_arrangers" (
    "version_id" VARCHAR(14) NOT NULL,
    "arranger_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_version_arrangers_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "fk_version_arrangers_arranger_id" FOREIGN KEY ("arranger_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_version_arrangers_version_id_arranger_id" UNIQUE ("version_id", "arranger_id")
);

CREATE TABLE "version_sources" (
    "version_id" VARCHAR(14) NOT NULL,
    "source_id" VARCHAR(14) NOT NULL,
    "structure" VARCHAR(32) NOT NULL,
    "details" VARCHAR NOT NULL,
    CONSTRAINT "fk_version_sources_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "fk_version_sources_source_id" FOREIGN KEY ("source_id") REFERENCES "source" ("id"),
    CONSTRAINT "uq_version_sources_version_id_source_id" UNIQUE ("version_id", "source_id")
);

CREATE TABLE "version_destructured_parts" (
    "version_id" VARCHAR(14) NOT NULL,
    "part" VARCHAR(32) NOT NULL,
    "melody" VARCHAR NOT NULL,
    "chords" VARCHAR NOT NULL,
    CONSTRAINT "fk_version_destructured_parts_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "uq_version_destructured_parts_version_id_part" UNIQUE ("version_id", "part")
);

CREATE TABLE "version_destructured_transitions" (
    "version_id" VARCHAR(14) NOT NULL,
    "from_parts" VARCHAR(32) NOT NULL,
    "to_parts" VARCHAR(32) NOT NULL,
    "melody" VARCHAR NOT NULL,
    "chords" VARCHAR NOT NULL,
    CONSTRAINT "fk_version_destructured_transitions_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "uq_version_destructured_transitions_version_id_from_parts_to_parts" UNIQUE ("version_id", "from_parts", "to_parts")
);

CREATE TABLE "set" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "name" VARCHAR NOT NULL,
    "kind" VARCHAR NOT NULL,
    "order" VARCHAR NOT NULL,
    "remark" VARCHAR NOT NULL,
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    "visibility" INT NOT NULL,
    CONSTRAINT "fk_set_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "set_conceptors" (
    "set_id" VARCHAR(14) NOT NULL,
    "conceptor_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_set_conceptors_set_id" FOREIGN KEY ("set_id") REFERENCES "set" ("id"),
    CONSTRAINT "fk_set_conceptors_conceptor_id" FOREIGN KEY ("conceptor_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_set_conceptors_set_id_conceptor_id" UNIQUE ("set_id", "conceptor_id")
);

CREATE TABLE "set_content" (
    "set_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "version_id" VARCHAR(14) NOT NULL,
    "version_parameter_transposition_semitones" INT,
    "version_parameter_first_bar" INT,
    "version_parameter_clef" VARCHAR,
    "version_parameter_structure" VARCHAR,
    "version_parameter_trivia" VARCHAR,
    "version_parameter_display_name" VARCHAR,
    "version_parameter_display_composer" VARCHAR,
    CONSTRAINT "fk_set_content_set_id" FOREIGN KEY ("set_id") REFERENCES "set" ("id"),
    CONSTRAINT "fk_set_content_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "uq_set_content_set_id_index" UNIQUE ("set_id", "index")
);

CREATE TABLE "set_viewers" (
    "set_id" VARCHAR(14) NOT NULL,
    "viewer_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_set_viewers_set_id" FOREIGN KEY ("set_id") REFERENCES "set" ("id"),
    CONSTRAINT "fk_set_viewers_viewer_id" FOREIGN KEY ("viewer_id") REFERENCES "user" ("id"),
    CONSTRAINT "uq_set_viewers_set_id_viewer_id" UNIQUE ("set_id", "viewer_id")
);

CREATE TABLE "set_owners" (
    "set_id" VARCHAR(14) NOT NULL,
    "owner_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_set_owners_set_id" FOREIGN KEY ("set_id") REFERENCES "set" ("id"),
    CONSTRAINT "fk_set_owners_owner_id" FOREIGN KEY ("owner_id") REFERENCES "user" ("id"),
    CONSTRAINT "uq_set_owners_set_id_owner_id" UNIQUE ("set_id", "owner_id")
);

CREATE TABLE "book" (
    "id" VARCHAR(14) NOT NULL PRIMARY KEY,
    "name" VARCHAR NOT NULL,
    "date" VARCHAR,
    "remark" VARCHAR NOT NULL,
    "scddb_id" INT,
    "created_at" TIMESTAMP NOT NULL,
    "modified_at" TIMESTAMP NOT NULL,
    "visibility" INT NOT NULL,
    CONSTRAINT "fk_book_id" FOREIGN KEY ("id") REFERENCES "globally_unique_id" ("id")
);

CREATE TABLE "book_authors" (
    "book_id" VARCHAR(14) NOT NULL,
    "author_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_book_authors_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_authors_author_id" FOREIGN KEY ("author_id") REFERENCES "person" ("id"),
    CONSTRAINT "uq_book_authors_book_id_author_id" UNIQUE ("book_id", "author_id")
);

CREATE TABLE "book_sources" (
    "book_id" VARCHAR(14) NOT NULL,
    "source_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_book_sources_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_sources_source_id" FOREIGN KEY ("source_id") REFERENCES "source" ("id"),
    CONSTRAINT "uq_book_sources_book_id_source_id" UNIQUE ("book_id", "source_id")
);

CREATE TABLE "book_content" (
    "book_id" VARCHAR(14) NOT NULL,
    "index" INT NOT NULL,
    "page_type" INT NOT NULL,
    "part_title" VARCHAR,
    "dance_id" VARCHAR(14),
    "set_id" VARCHAR(14), -- standalone or within dance
    "set_parameter_display_name" VARCHAR,
    "set_parameter_display_conceptor" VARCHAR,
    "set_parameter_display_kind" VARCHAR,
    "set_parameter_version_parameter_transposition_semitones" INT,
    "set_parameter_version_parameter_first_bar" INT,
    "set_parameter_version_parameter_clef" VARCHAR,
    "set_parameter_version_parameter_structure" VARCHAR,
    "set_parameter_version_parameter_trivia" VARCHAR,
    "set_parameter_version_parameter_display_name" VARCHAR,
    "set_parameter_version_parameter_display_composer" VARCHAR,
    CONSTRAINT "fk_book_content_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_content_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dance" ("id"),
    CONSTRAINT "fk_book_content_set_id" FOREIGN KEY ("set_id") REFERENCES "set" ("id"),
    CONSTRAINT "uq_book_content_book_id_index" UNIQUE ("book_id", "index")
);

CREATE TABLE "book_content_versions" ( -- standalone or within dance
    "book_id" VARCHAR(14) NOT NULL,
    "content_index" INT NOT NULL,
    "index" INT NOT NULL,
    "version_id" VARCHAR(14) NOT NULL,
    "version_parameter_transposition_semitones" INT,
    "version_parameter_first_bar" INT,
    "version_parameter_clef" VARCHAR,
    "version_parameter_structure" VARCHAR,
    "version_parameter_trivia" VARCHAR,
    "version_parameter_display_name" VARCHAR,
    "version_parameter_display_composer" VARCHAR,
    CONSTRAINT "fk_book_content_versions_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_content_versions_version_id" FOREIGN KEY ("version_id") REFERENCES "version" ("id"),
    CONSTRAINT "uq_book_content_versions_book_id_content_index_index" UNIQUE ("book_id", "content_index", "index")
);

CREATE TABLE "book_viewers" (
    "book_id" VARCHAR(14) NOT NULL,
    "viewer_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_book_viewers_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_viewers_viewer_id" FOREIGN KEY ("viewer_id") REFERENCES "user" ("id"),
    CONSTRAINT "uq_book_viewers_book_id_viewer_id" UNIQUE ("book_id", "viewer_id")
);

CREATE TABLE "book_owners" (
    "book_id" VARCHAR(14) NOT NULL,
    "owner_id" VARCHAR(14) NOT NULL,
    CONSTRAINT "fk_book_owners_book_id" FOREIGN KEY ("book_id") REFERENCES "book" ("id"),
    CONSTRAINT "fk_book_owners_owner_id" FOREIGN KEY ("owner_id") REFERENCES "user" ("id"),
    CONSTRAINT "uq_book_owners_book_id_owner_id" UNIQUE ("book_id", "owner_id")
);
