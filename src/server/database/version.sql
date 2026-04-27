-- @get
SELECT "json"
FROM "version"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "version";

-- @update
INSERT INTO "version" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "version"
WHERE "id" = @id;
