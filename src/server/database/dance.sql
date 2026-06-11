-- @get
SELECT
    "name",
    "kind",
    "two_chords",
    "scddb_id",
    "disambiguation",
    "date",
    "created_at",
    "modified_at"
FROM "dance"
JOIN "entry" ON "dance"."id" = "entry"."id"
WHERE "dance"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @get_all
SELECT
    "dance"."id",
    "name",
    "kind",
    "two_chords",
    "scddb_id",
    "disambiguation",
    "date",
    "created_at",
    "modified_at"
FROM "dance"
JOIN "entry" ON "dance"."id" = "entry"."id";

-- @create
INSERT INTO "dance" (
    "id",
    "name",
    "kind",
    "two_chords",
    "scddb_id",
    "disambiguation",
    "date"
) VALUES (
    @id,
    @name,
    @kind,
    @two_chords,
    @scddb_id,
    @disambiguation,
    @date
);

-- @update
UPDATE "dance"
SET
    "name" = @name,
    "kind" = @kind,
    "two_chords" = @two_chords,
    "scddb_id" = @scddb_id,
    "disambiguation" = @disambiguation,
    "date" = @date
WHERE "id" = @id;

-- @delete
DELETE FROM "dance"
WHERE "id" = @id;

-- @get_extra_names
SELECT
    "extra_name"
FROM "dance_extra_names"
WHERE "dance_id" = @dance_id
ORDER BY "extra_name";

-- @get_all_extra_names
SELECT
    "dance_id",
    "extra_name"
FROM "dance_extra_names"
ORDER BY "dance_id", "extra_name";

-- @delete_all_extra_names
DELETE FROM "dance_extra_names"
WHERE "dance_id" = @dance_id;

-- @add_one_extra_name
INSERT INTO "dance_extra_names" (
    "dance_id",
    "extra_name"
) VALUES (
    @dance_id,
    @extra_name
);

-- @get_devisers
SELECT
    "deviser_id"
FROM "dance_devisers"
WHERE "dance_id" = @dance_id
ORDER BY "index";

-- @get_all_devisers
SELECT
    "dance_id",
    "deviser_id"
FROM "dance_devisers"
ORDER BY "dance_id", "index";

-- @delete_all_devisers
DELETE FROM "dance_devisers"
WHERE "dance_id" = @dance_id;

-- @add_one_deviser
INSERT INTO "dance_devisers" (
    "dance_id",
    "index",
    "deviser_id"
) VALUES (
    @dance_id,
    @index,
    @deviser_id
);

-- @search
SELECT
    CASE WHEN @needle = '' THEN 1.0 ELSE word_similarity(@needle, "name") END AS "score",
    "dance"."id",
    "name",
    "kind",
    "disambiguation"
FROM "dance"
WHERE (@needle = '' OR @needle <% "name")
ORDER BY "score" DESC, "name" ASC;

-- @get_all_devisers_new
SELECT
    "dance_id",
    "person"."id",
    "person"."name"
FROM "dance_devisers"
JOIN "person"
ON "dance_devisers"."deviser_id" = "person"."id"
ORDER BY "index";
