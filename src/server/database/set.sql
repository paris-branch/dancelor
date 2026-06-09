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
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "kind",
    "order",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
FROM "set";

-- @create
INSERT INTO "set" (
    "id",
    "name",
    "kind",
    "order",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
) VALUES (
    @id,
    @name,
    @kind,
    @order,
    @remark,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP,
    @visibility
);

-- @update
UPDATE "set"
SET
    "name" = @name,
    "kind" = @kind,
    "order" = @order,
    "remark" = @remark,
    "modified_at" = CURRENT_TIMESTAMP,
    "visibility" = @visibility
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

-- @get_viewers
SELECT "viewer_id"
FROM "set_viewers"
WHERE "set_id" = @set_id;

-- @get_all_viewers
SELECT
    "set_id",
    "viewer_id"
FROM "set_viewers";

-- @delete_all_viewers
DELETE FROM "set_viewers"
WHERE "set_id" = @set_id;

-- @add_one_viewer
INSERT INTO "set_viewers" (
    "set_id",
    "viewer_id"
) VALUES (
    @set_id,
    @viewer_id
);

-- @get_owners
SELECT "owner_id"
FROM "set_owners"
WHERE "set_id" = @set_id;

-- @get_all_owners
SELECT
    "set_id",
    "owner_id"
FROM "set_owners";

-- @delete_all_owners
DELETE FROM "set_owners"
WHERE "set_id" = @set_id;

-- @add_one_owner
INSERT INTO "set_owners" (
    "set_id",
    "owner_id"
) VALUES (
    @set_id,
    @owner_id
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

-- @search
SELECT
    "search"."score",
    "set"."id",
    "name",
    "kind",
    "permission"
FROM (
    SELECT
        "set"."id",
        CASE
            WHEN @needle = '' THEN 1.0
            ELSE word_similarity(@needle, "name")
        END AS "score",
        CASE
            WHEN "set"."visibility" = 1 THEN 'Everyone'
            WHEN "set_owners"."owner_id" IS NOT NULL THEN 'Owner'
            WHEN "set"."visibility" = 2 AND "set_viewers"."viewer_id" IS NOT NULL THEN 'Viewer'
            WHEN "user"."role" = 2 AND "user"."omniscience" THEN 'Omniscient_administrator'
            ELSE NULL
        END AS "permission"
    FROM "set"
    LEFT JOIN "set_owners" ON "set_owners"."set_id" = "set"."id" AND "set_owners"."owner_id" = @user_id
    LEFT JOIN "set_viewers" ON "set_viewers"."set_id" = "set"."id" AND "set_viewers"."viewer_id" = @user_id
    LEFT JOIN "user" ON "user"."id" = @user_id
) AS "search"
JOIN "set" ON "set"."id" = "search"."id"
WHERE "search"."score" >= @threshold
  AND "search"."permission" IS NOT NULL
ORDER BY "score" DESC, "name" ASC;

-- @get_all_conceptors_new
SELECT
    "set_id",
    "person"."id",
    "person"."name"
FROM "set_conceptors"
JOIN "person" ON "set_conceptors"."conceptor_id" = "person"."id";

-- @get_all_tunes_new
SELECT
    "set_id",
    "version"."id",
    "tune"."name"
FROM "set_content"
JOIN "version" ON "set_content"."version_id" = "version"."id"
JOIN "tune" ON "version"."tune_id" = "tune"."id"
ORDER BY "index";
