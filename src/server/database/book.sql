-- @get
SELECT
    "name",
    "date",
    "remark",
    "scddb_id",
    "created_at",
    "modified_at",
    "visibility"
FROM "book"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "date",
    "remark",
    "scddb_id",
    "created_at",
    "modified_at",
    "visibility"
FROM "book";

-- @create
INSERT INTO "book" (
    "id",
    "name",
    "date",
    "remark",
    "scddb_id",
    "created_at",
    "modified_at",
    "visibility"
) VALUES (
    @id,
    @name,
    @date,
    @remark,
    @scddb_id,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP,
    @visibility
);

-- @update
UPDATE "book"
SET
    "name" = @name,
    "date" = @date,
    "remark" = @remark,
    "scddb_id" = @scddb_id,
    "modified_at" = CURRENT_TIMESTAMP,
    "visibility" = @visibility
WHERE "id" = @id;

-- @delete
DELETE FROM "book"
WHERE "id" = @id;

-- @get_authors
SELECT "author_id"
FROM "book_authors"
WHERE "book_id" = @book_id;

-- @get_all_authors
SELECT
    "book_id",
    "author_id"
FROM "book_authors";

-- @delete_all_authors
DELETE FROM "book_authors"
WHERE "book_id" = @book_id;

-- @add_one_author
INSERT INTO "book_authors" (
    "book_id",
    "author_id"
) VALUES (
    @book_id,
    @author_id
);

-- @get_sources
SELECT "source_id"
FROM "book_sources"
WHERE "book_id" = @book_id;

-- @get_all_sources
SELECT
    "book_id",
    "source_id"
FROM "book_sources";

-- @delete_all_sources
DELETE FROM "book_sources"
WHERE "book_id" = @book_id;

-- @add_one_source
INSERT INTO "book_sources" (
    "book_id",
    "source_id"
) VALUES (
    @book_id,
    @source_id
);

-- @get_viewers
SELECT "viewer_id"
FROM "book_viewers"
WHERE "book_id" = @book_id;

-- @get_all_viewers
SELECT
    "book_id",
    "viewer_id"
FROM "book_viewers";

-- @delete_all_viewers
DELETE FROM "book_viewers"
WHERE "book_id" = @book_id;

-- @add_one_viewer
INSERT INTO "book_viewers" (
    "book_id",
    "viewer_id"
) VALUES (
    @book_id,
    @viewer_id
);

-- @get_owners
SELECT "owner_id"
FROM "book_owners"
WHERE "book_id" = @book_id;

-- @get_all_owners
SELECT
    "book_id",
    "owner_id"
FROM "book_owners";

-- @delete_all_owners
DELETE FROM "book_owners"
WHERE "book_id" = @book_id;

-- @add_one_owner
INSERT INTO "book_owners" (
    "book_id",
    "owner_id"
) VALUES (
    @book_id,
    @owner_id
);

-- @get_content
SELECT
    "index",
    "page_type",
    "part_title",
    "dance_id",
    "set_id",
    "set_parameter_display_name",
    "set_parameter_display_conceptor",
    "set_parameter_display_kind",
    "set_parameter_version_parameter_transposition_semitones",
    "set_parameter_version_parameter_first_bar",
    "set_parameter_version_parameter_clef",
    "set_parameter_version_parameter_structure",
    "set_parameter_version_parameter_trivia",
    "set_parameter_version_parameter_display_name",
    "set_parameter_version_parameter_display_composer"
FROM "book_content"
WHERE "book_id" = @book_id
ORDER BY "index";

-- @get_all_content
SELECT
    "book_id",
    "index",
    "page_type",
    "part_title",
    "dance_id",
    "set_id",
    "set_parameter_display_name",
    "set_parameter_display_conceptor",
    "set_parameter_display_kind",
    "set_parameter_version_parameter_transposition_semitones",
    "set_parameter_version_parameter_first_bar",
    "set_parameter_version_parameter_clef",
    "set_parameter_version_parameter_structure",
    "set_parameter_version_parameter_trivia",
    "set_parameter_version_parameter_display_name",
    "set_parameter_version_parameter_display_composer"
FROM "book_content"
ORDER BY "book_id", "index";

-- @delete_all_content
DELETE FROM "book_content"
WHERE "book_id" = @book_id;

-- @add_one_content_item
INSERT INTO "book_content" (
    "book_id",
    "index",
    "page_type",
    "part_title",
    "dance_id",
    "set_id",
    "set_parameter_display_name",
    "set_parameter_display_conceptor",
    "set_parameter_display_kind",
    "set_parameter_version_parameter_transposition_semitones",
    "set_parameter_version_parameter_first_bar",
    "set_parameter_version_parameter_clef",
    "set_parameter_version_parameter_structure",
    "set_parameter_version_parameter_trivia",
    "set_parameter_version_parameter_display_name",
    "set_parameter_version_parameter_display_composer"
) VALUES (
    @book_id,
    @index,
    @page_type,
    @part_title,
    @dance_id,
    @set_id,
    @set_parameter_display_name,
    @set_parameter_display_conceptor,
    @set_parameter_display_kind,
    @set_parameter_version_parameter_transposition_semitones,
    @set_parameter_version_parameter_first_bar,
    @set_parameter_version_parameter_clef,
    @set_parameter_version_parameter_structure,
    @set_parameter_version_parameter_trivia,
    @set_parameter_version_parameter_display_name,
    @set_parameter_version_parameter_display_composer
);

-- @get_content_versions
SELECT
    "content_index",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "book_content_versions"
WHERE "book_id" = @book_id
ORDER BY "content_index", "index";

-- @get_all_content_versions
SELECT
    "book_id",
    "content_index",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "book_content_versions"
ORDER BY "book_id", "content_index", "index";

-- @delete_all_content_versions
DELETE FROM "book_content_versions"
WHERE "book_id" = @book_id;

-- @add_one_content_version
INSERT INTO "book_content_versions" (
    "book_id",
    "content_index",
    "index",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
) VALUES (
    @book_id,
    @content_index,
    @index,
    @version_id,
    @version_parameter_transposition_semitones,
    @version_parameter_first_bar,
    @version_parameter_clef,
    @version_parameter_structure,
    @version_parameter_trivia,
    @version_parameter_display_name,
    @version_parameter_display_composer
);
