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
    "search"."score",
    "person"."id",
    "name"
FROM (
    SELECT
        "id",
	@needle {Some { word_similarity(@needle, "name") } | None { '1' } } AS "score"
    FROM "person"
) AS "search"
JOIN "person"
ON "person"."id" = "search"."id"
WHERE "search"."score" >= @threshold
ORDER BY "score" DESC, "name" ASC;
