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

-- NEW MODELS

-- @get_rows
WITH "sources" AS &get_source_rows
SELECT "sources".*
FROM "sources"
WHERE "id" IN @ids;

-- @get_view
WITH "sources" AS &get_source_views
SELECT "sources".*
FROM "sources"
WHERE "id" = @id;

-- @search
WITH "sources" AS &get_source_rows
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "sources".*
FROM "sources"
WHERE
    (@terms = '' OR @terms <% "name")
    AND @editor { Some { EXISTS (SELECT 1 FROM "source_editors" WHERE "source_id" = "sources"."id" AND "person_id" IN @editor) } | None { TRUE } }
ORDER BY "score" DESC, "name" ASC;

-- @get_editors_for
WITH "persons" AS &get_person_rows
SELECT
    "source_id",
    "persons".*
FROM "source_editors"
JOIN "persons" ON "source_editors"."person_id" = "persons"."id"
WHERE @source_ids { One_of { "source_id" IN @source_ids } | All { TRUE } };
