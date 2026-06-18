-- @get
SELECT
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public",
    "created_at",
    "modified_at"
FROM "person"
JOIN "entry" ON "person"."id" = "entry"."id"
WHERE "person"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @create
INSERT INTO "person" (
    "id",
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public"
)
VALUES (
    @id,
    @name,
    @scddb_id,
    @composed_tunes_are_public,
    @published_tunes_are_public
);

-- @update
UPDATE "person"
SET
    "name" = @name,
    "scddb_id" = @scddb_id,
    "composed_tunes_are_public" = @composed_tunes_are_public,
    "published_tunes_are_public" = @published_tunes_are_public
WHERE "id" = @id;

-- @delete
DELETE FROM "person"
WHERE "id" = @id;

-- NEW MODELS

-- @get_row
SELECT "name"
FROM "person"
WHERE "id" = @id;

-- @get_rows
SELECT
    "id",
    "name"
FROM "person"
WHERE "id" IN @ids;

-- @get_view
SELECT
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public"
FROM "person"
WHERE "id" = @id;

-- @get_row_for_user
SELECT
    "person"."id",
    "person"."name"
FROM "person"
JOIN "user" ON "person"."id" = "user"."person_id"
WHERE "user"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @search
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "id",
    "name"
FROM "person"
WHERE (@terms = '' OR @terms <% "name")
ORDER BY "score" DESC, "name" ASC;
