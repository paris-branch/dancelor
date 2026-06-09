-- @get
SELECT
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public",
    "created_at",
    "modified_at"
FROM "person"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public",
    "created_at",
    "modified_at"
FROM "person";

-- @create
INSERT INTO "person" (
    "id",
    "name",
    "scddb_id",
    "composed_tunes_are_public",
    "published_tunes_are_public",
    "created_at",
    "modified_at"
)
VALUES (
    @id,
    @name,
    @scddb_id,
    @composed_tunes_are_public,
    @published_tunes_are_public,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- @update
UPDATE "person"
SET
    "name" = @name,
    "scddb_id" = @scddb_id,
    "composed_tunes_are_public" = @composed_tunes_are_public,
    "published_tunes_are_public" = @published_tunes_are_public,
    "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;

-- @delete
DELETE FROM "person"
WHERE "id" = @id;

-- @search
SELECT
    CASE WHEN @needle = '' THEN 1.0 ELSE word_similarity(@needle, "name") END AS "score",
    "id",
    "name"
FROM "person"
WHERE (@needle = '' OR @needle <% "name")
ORDER BY "score" DESC, "name" ASC;
