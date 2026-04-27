-- @get
SELECT "json"
FROM "source"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "source";

-- @update
INSERT INTO "source" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "source"
WHERE "id" = @id;

-- @get_cover
SELECT "cover"
FROM "source"
WHERE "id" = @id;
