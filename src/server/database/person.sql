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
