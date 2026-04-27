-- @get
SELECT "json"
FROM "set"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "set";

-- @update
INSERT INTO "set" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "set"
WHERE "id" = @id;
