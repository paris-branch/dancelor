-- @get
SELECT
    "name",
    "short_name",
    "scddb_id",
    "description",
    "date",
    "created_at",
    "modified_at"
FROM "source"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "short_name",
    "scddb_id",
    "description",
    "date",
    "created_at",
    "modified_at"
FROM "source";

-- @create
INSERT INTO "source" (
    "id",
    "name",
    "short_name",
    "scddb_id",
    "description",
    "date",
    "created_at",
    "modified_at"
) VALUES (
    @id,
    @name,
    @short_name,
    @scddb_id,
    @description,
    @date,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- @get_editors
SELECT
    "person_id"
FROM "source_editors"
WHERE "source_id" = @source_id;

-- @get_all_editors
SELECT
    "source_id",
    "person_id"
FROM "source_editors";

-- @delete_all_editors
DELETE FROM "source_editors"
WHERE "source_id" = @source_id;

-- @add_one_editor
INSERT INTO "source_editors" (
    "source_id",
    "person_id"
) VALUES (
    @source_id,
    @person_id
);

-- @update
UPDATE "source"
SET
    "name" = @name,
    "short_name" = @short_name,
    "scddb_id" = @scddb_id,
    "description" = @description,
    "date" = @date,
    "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;

-- @delete
DELETE FROM "source"
WHERE "id" = @id;

-- @get_cover
SELECT "cover"
FROM "source"
WHERE "id" = @id;

-- @search
SELECT
    CASE WHEN @needle = '' THEN 1.0 ELSE word_similarity(@needle, "name") END AS "score",
    "source"."id",
    "name",
    "date"
FROM "source"
WHERE (CASE WHEN @needle = '' THEN 1.0 ELSE word_similarity(@needle, "name") END) >= @threshold
ORDER BY "score" DESC, "name" ASC;

-- @get_all_editors_new
SELECT
    "source_id",
    "person"."id",
    "person"."name"
FROM "source_editors"
JOIN "person" ON "source_editors"."person_id" = "person"."id";
