-- @get
SELECT "json"
FROM "person"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "person";

-- @update
INSERT INTO "person" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "person"
WHERE "id" = @id;
