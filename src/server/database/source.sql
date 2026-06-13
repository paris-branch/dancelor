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
JOIN "entry" ON "source"."id" = "entry"."id"
WHERE "source"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @get_all
SELECT
    "source"."id",
    "name",
    "short_name",
    "scddb_id",
    "description",
    "date",
    "created_at",
    "modified_at"
FROM "source"
JOIN "entry" ON "source"."id" = "entry"."id";

-- @create
INSERT INTO "source" (
    "id",
    "name",
    "short_name",
    "scddb_id",
    "description",
    "date"
) VALUES (
    @id,
    @name,
    @short_name,
    @scddb_id,
    @description,
    @date
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
    "date" = @date
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
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "source"."id",
    "name",
    "date"
FROM "source"
WHERE
    (@terms = '' OR @terms <% "name")
    AND @editor { Some { EXISTS (SELECT 1 FROM "source_editors" WHERE "source_id" = "source"."id" AND "person_id" IN @editor) } | None { TRUE } }
ORDER BY "score" DESC, "name" ASC;

-- @get_all_editors_new
SELECT
    "source_id",
    "person"."id",
    "person"."name"
FROM "source_editors"
JOIN "person" ON "source_editors"."person_id" = "person"."id";
