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

-- @get_all
SELECT
    "person"."id",
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public",
    "created_at",
    "modified_at"
FROM "person"
JOIN "entry" ON "person"."id" = "entry"."id";

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

-- @search
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "id",
    "name"
FROM "person"
WHERE (@terms = '' OR @terms <% "name")
ORDER BY "score" DESC, "name" ASC;
