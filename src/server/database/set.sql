-- @get
SELECT "yaml"
FROM "set"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "yaml"
FROM "set";

-- @update
INSERT INTO "set" ("id", "yaml")
VALUES (@id, @yaml)
ON CONFLICT ("id") DO UPDATE SET "yaml" = EXCLUDED."yaml";

-- @delete
DELETE FROM "set"
WHERE "id" = @id;
