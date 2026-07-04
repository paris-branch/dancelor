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

-- NEW MODELS

-- @get_rows
WITH "dances" AS &get_dance_rows
SELECT *
FROM "dances"
WHERE @ids { One_of { "id" IN @ids } | All { TRUE } };

-- @get_view
WITH "dances" AS &get_dance_views
SELECT *
FROM "dances"
WHERE "id" = @id;

-- @search
WITH "dances" AS &get_dance_rows
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "dances".*
FROM "dances"
WHERE
    (@terms = '' OR @terms <% "name")
    AND @deviser { Some { EXISTS (SELECT 1 FROM "dance_devisers" WHERE "dance_id" = "dances"."id" AND "deviser_id" IN @deviser) } | None { TRUE } }
ORDER BY "score" DESC, "name" ASC;

-- @get_extra_names_for
SELECT
    "dance_id",
    "extra_name"
FROM "dance_extra_names"
WHERE @dance_ids { One_of { "dance_id" IN @dance_ids } | All { TRUE } }
ORDER BY "extra_name";

-- @get_devisers_for
WITH "persons" AS &get_person_rows
SELECT
    "dance_id",
    "persons".*
FROM "dance_devisers"
JOIN "persons" ON "dance_devisers"."deviser_id" = "persons"."id"
WHERE @dance_ids { One_of { "dance_id" IN @dance_ids } | All { TRUE } }
ORDER BY "index";

-- @get_tunes_for
SELECT
    "dance_id",
    "id",
    "name",
    "kind"
FROM "recommended_tunes"
JOIN "tune" ON "recommended_tunes"."tune_id" = "tune"."id"
WHERE @dance_ids { One_of { "recommended_tunes"."dance_id" IN @dance_ids } | All { TRUE } };

-- @get_composers_for_tunes_for
SELECT
    "tune_composers"."tune_id",
    "person"."id",
    "person"."name"
FROM "recommended_tunes"
JOIN "tune_composers" ON "recommended_tunes"."tune_id" = "tune_composers"."tune_id"
JOIN "person" ON "tune_composers"."composer_id" = "person"."id"
WHERE @dance_ids { One_of { "recommended_tunes"."dance_id" IN @dance_ids } | All { TRUE } }
ORDER BY "index";
