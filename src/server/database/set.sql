-- @get
SELECT
    "name",
    "kind",
    "order",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
FROM "set"
JOIN "entry" ON "set"."id" = "entry"."id"
WHERE "set"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @create
INSERT INTO "set" (
    "id",
    "name",
    "kind",
    "order",
    "remark"
) VALUES (
    @id,
    @name,
    @kind,
    @order,
    @remark
);

-- @update
UPDATE "set"
SET
    "name" = @name,
    "kind" = @kind,
    "order" = @order,
    "remark" = @remark
WHERE "id" = @id;

-- @delete
DELETE FROM "set"
WHERE "id" = @id;

-- @get_conceptors
SELECT "conceptor_id"
FROM "set_conceptors"
WHERE "set_id" = @set_id;

-- @get_all_conceptors
SELECT
    "set_id",
    "conceptor_id"
FROM "set_conceptors";

-- @delete_all_conceptors
DELETE FROM "set_conceptors"
WHERE "set_id" = @set_id;

-- @add_one_conceptor
INSERT INTO "set_conceptors" (
    "set_id",
    "conceptor_id"
) VALUES (
    @set_id,
    @conceptor_id
);

-- @get_content
SELECT
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "set_content"
WHERE "set_id" = @set_id
ORDER BY "index";

-- @get_all_content
SELECT
    "set_id",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "set_content"
ORDER BY "set_id", "index";

-- @delete_all_content
DELETE FROM "set_content"
WHERE "set_id" = @set_id;

-- @add_one_content_item
INSERT INTO "set_content" (
    "set_id",
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
    @set_id,
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

-- NEW MODELS

-- @get_row
SELECT * FROM (
    SELECT
        "set"."name",
        "set"."kind",
        CASE
            WHEN "entry"."visibility" = 'Everyone' THEN 'Everyone'
            WHEN "entry_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 'Administrator' AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "set"
    JOIN "entry" ON "entry"."id" = "set"."id"
    LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "user" ON "user"."id" = (@user_id :: TEXT NULL)
    WHERE "set"."id" = @id
) AS "set+"
WHERE "set+"."permission" IS NOT NULL
LIMIT 1; -- NOTE: to help sqlgg

-- @get_rows
SELECT * FROM (
    SELECT
        "set"."id",
        "set"."name",
        "set"."kind",
        CASE
            WHEN "entry"."visibility" = 'Everyone' THEN 'Everyone'
            WHEN "entry_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 'Administrator' AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "set"
    JOIN "entry" ON "entry"."id" = "set"."id"
    LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "user" ON "user"."id" = (@user_id :: TEXT NULL)
    WHERE "set"."id" IN @ids
) AS "set+"
WHERE "set+"."permission" IS NOT NULL;

-- @get_view
SELECT * FROM (
    SELECT
        "set"."name",
        "set"."kind",
	"set"."order",
	"set"."remark",
        CASE
            WHEN "entry"."visibility" = 'Everyone' THEN 'Everyone'
            WHEN "entry_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 'Administrator' AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "set"
    JOIN "entry" ON "entry"."id" = "set"."id"
    LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "user" ON "user"."id" = (@user_id :: TEXT NULL)
    WHERE "set"."id" = @id
) AS "set+"
WHERE "set+"."permission" IS NOT NULL
LIMIT 1; -- NOTE: to help sqlgg

-- @search
SELECT * FROM (
    SELECT
        CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
        "set"."id",
        "set"."name",
        "set"."kind",
        CASE
            WHEN "entry"."visibility" = 'Everyone' THEN 'Everyone'
            WHEN "entry_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 'Administrator' AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "set"
    JOIN "entry" ON "entry"."id" = "set"."id"
    LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = (@user_id :: TEXT NULL)
    LEFT JOIN "user" ON "user"."id" = (@user_id :: TEXT NULL)
    WHERE
        (@terms = '' OR @terms <% "name")
        AND @conceptor { Some { EXISTS (SELECT 1 FROM "set_conceptors" WHERE "set_id" = "set"."id" AND "conceptor_id" IN @conceptor) } | None { TRUE } }
	AND @contains_version { Some { EXISTS (SELECT 1 FROM "set_content" WHERE "set_id" = "set"."id" AND "version_id" IN @contains_version ) } | None { TRUE } }
	AND @contains_tune { Some { EXISTS (SELECT 1 FROM "set_content" JOIN "version" ON "set_content"."version_id" = "version"."id" WHERE "set_content"."set_id" = "set"."id" AND "version"."tune_id" IN @contains_tune ) } | None { TRUE } }
) AS "set+"
WHERE "set+"."permission" IS NOT NULL
ORDER BY "score" DESC, "name" ASC;

-- @get_conceptors_for
SELECT
    "set_id",
    "person"."id",
    "person"."name"
FROM "set_conceptors"
JOIN "person" ON "set_conceptors"."conceptor_id" = "person"."id"
WHERE @set_ids { One_of { "set_id" IN @set_ids } | All { TRUE } };

-- @get_tunes_for
SELECT
    "set_id",
    "version"."id",
    "tune"."name"
FROM "set_content"
JOIN "version" ON "set_content"."version_id" = "version"."id"
JOIN "tune" ON "version"."tune_id" = "tune"."id"
WHERE @set_ids { One_of { "set_id" IN @set_ids } | All { TRUE } }
ORDER BY "index";

-- @get_content_for
SELECT
    -- ids
    "set_id",
    "version_id",
    "tune_id",
    -- version
    "version"."disambiguation" AS "version_disambiguation",
    "version"."monolithic_bars" AS "version_monolithic_bars",
    "version"."monolithic_or_default_structure" AS "version_monolithic_or_default_structure",
    -- tune
    "name" AS "tune_name",
    "kind" AS "tune_kind",
    -- version parameters
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "set_content"
JOIN "version" ON "set_content"."version_id" = "version"."id"
JOIN "tune" ON "version"."tune_id" = "tune"."id"
WHERE @set_ids { One_of { "set_id" IN @set_ids } | All { TRUE } }
ORDER BY "index";
