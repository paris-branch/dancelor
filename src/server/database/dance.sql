-- @get
SELECT "json"
FROM "dance"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "dance";

-- @update
INSERT INTO "dance" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "dance"
WHERE "id" = @id;
