-- @get
SELECT "yaml"
FROM "book"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "yaml"
FROM "book";

-- @update
INSERT INTO "book" ("id", "yaml")
VALUES (@id, @yaml)
ON CONFLICT ("id") DO UPDATE SET "yaml" = EXCLUDED."yaml";

-- @delete
DELETE FROM "book"
WHERE "id" = @id;
