-- @get
SELECT "json"
FROM "book"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "json"
FROM "book";

-- @update
INSERT INTO "book" ("id", "json")
VALUES (@id, @json)
ON CONFLICT ("id") DO UPDATE SET "json" = @json;

-- @delete
DELETE FROM "book"
WHERE "id" = @id;
