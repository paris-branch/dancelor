-- @get
SELECT "json"
FROM "user"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "user";

-- @update
INSERT INTO "user" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "user"
WHERE "id" = @id;
