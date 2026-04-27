-- @get
SELECT "yaml"
FROM "version"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "yaml"
FROM "version";

-- @update
INSERT INTO "version" ("id", "yaml")
VALUES (@id, @yaml)
ON CONFLICT ("id") DO UPDATE SET "yaml" = EXCLUDED."yaml";

-- @delete
DELETE FROM "version"
WHERE "id" = @id;
