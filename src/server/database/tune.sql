-- @get
SELECT "json"
FROM "tune"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "tune";

-- @update
INSERT INTO "tune" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "tune"
WHERE "id" = @id;
