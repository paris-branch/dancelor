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
JOIN "entry" ON "book"."id" = "entry"."id"
WHERE "book"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @get_all
SELECT
    "book"."id",
    "name",
    "date",
    "remark",
    "scddb_id",
    "created_at",
    "modified_at",
    "visibility"
FROM "book"
JOIN "entry" ON "book"."id" = "entry"."id";

-- @create
INSERT INTO "book" (
    "id",
    "name",
    "date",
    "remark",
    "scddb_id"
) VALUES (
    @id,
    @name,
    @date,
    @remark,
    @scddb_id
);

-- @update
UPDATE "book"
SET
    "name" = @name,
    "date" = @date,
    "remark" = @remark,
    "scddb_id" = @scddb_id
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

-- @search
SELECT * FROM (
    SELECT
        CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
        "book"."id",
        "name",
        "date",
        CASE
            WHEN "entry"."visibility" = 'Everyone' THEN 'Everyone'
            WHEN "entry_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 'Administrator' AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "book"
    JOIN "entry" ON "entry"."id" = "book"."id"
    LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = @user_id
    LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = @user_id
    LEFT JOIN "user" ON "user"."id" = @user_id
    WHERE
	(@terms = '' OR @terms <% "name")
        AND @author { Some { EXISTS (SELECT 1 FROM "book_authors" WHERE "book_id" = "book"."id" AND "author_id" IN @author) } | None { TRUE } }
) AS "book+"
WHERE "book+"."permission" IS NOT NULL
ORDER BY "score" DESC, "name" ASC;

-- @get_all_authors_new
SELECT
    "book_id",
    "person"."id",
    "person"."name"
FROM "book_authors"
JOIN "person" ON "book_authors"."author_id" = "person"."id";
