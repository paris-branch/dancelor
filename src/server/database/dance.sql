-- @get
SELECT "yaml"
FROM "dance"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "yaml"
FROM "dance";

-- @update
INSERT INTO "dance" ("id", "yaml")
VALUES (@id, @yaml)
ON CONFLICT ("id") DO UPDATE SET "yaml" = EXCLUDED."yaml";

-- @delete
DELETE FROM "dance"
WHERE "id" = @id;
