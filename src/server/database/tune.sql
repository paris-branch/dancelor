-- @get
SELECT
    "name",
    "kind",
    "remark",
    "scddb_id",
    "date",
    "created_at",
    "modified_at"
FROM "tune"
JOIN "entry" ON "tune"."id" = "entry"."id"
WHERE "tune"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @create
INSERT INTO "tune" (
    "id",
    "name",
    "kind",
    "remark",
    "scddb_id",
    "date"
) VALUES (
    @id,
    @name,
    @kind,
    @remark,
    @scddb_id,
    @date
);

-- @update
UPDATE "tune"
SET
    "name" = @name,
    "kind" = @kind,
    "remark" = @remark,
    "scddb_id" = @scddb_id,
    "date" = @date
WHERE "id" = @id;

-- @delete
DELETE FROM "tune"
WHERE "id" = @id;

-- @get_extra_names
SELECT
    "extra_name"
FROM "tune_extra_names"
WHERE "tune_id" = @tune_id
ORDER BY "extra_name";

-- @get_all_extra_names
SELECT
    "tune_id",
    "extra_name"
FROM "tune_extra_names"
ORDER BY "tune_id", "extra_name";

-- @delete_all_extra_names
DELETE FROM "tune_extra_names"
WHERE "tune_id" = @tune_id;

-- @add_one_extra_name
INSERT INTO "tune_extra_names" (
    "tune_id",
    "extra_name"
) VALUES (
    @tune_id,
    @extra_name
);

-- @get_composers
SELECT
    "composer_id",
    "details"
FROM "tune_composers"
WHERE "tune_id" = @tune_id
ORDER BY "index";

-- @get_all_composers
SELECT
    "tune_id",
    "composer_id",
    "details"
FROM "tune_composers"
ORDER BY "tune_id", "index";

-- @delete_all_composers
DELETE FROM "tune_composers"
WHERE "tune_id" = @tune_id;

-- @add_one_composer
INSERT INTO "tune_composers" (
    "tune_id",
    "index",
    "composer_id",
    "details"
) VALUES (
    @tune_id,
    @index,
    @composer_id,
    @details
);

-- @get_dances
SELECT
    "dance_id"
FROM "recommended_tunes"
WHERE "tune_id" = @tune_id;

-- @get_all_dances
SELECT
    "tune_id",
    "dance_id"
FROM "recommended_tunes";

-- @delete_all_dances
DELETE FROM "recommended_tunes"
WHERE "tune_id" = @tune_id;

-- @add_one_dance
INSERT INTO "recommended_tunes" (
    "tune_id",
    "dance_id"
) VALUES (
    @tune_id,
    @dance_id
);

-- NEW MODELS

-- @get_row
SELECT
    "name",
    "kind"
FROM "tune"
WHERE "id" = @id;

-- @get_rows
SELECT
    "id",
    "name",
    "kind"
FROM "tune"
WHERE "id" IN @ids;

-- @get_view
SELECT
    "name",
    "kind",
    "remark",
    "scddb_id",
    "date"
FROM "tune"
WHERE "id" = @id;

-- @search
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "name") END AS "score",
    "tune"."id",
    "name",
    "kind"
FROM "tune"
WHERE
    (@terms = '' OR @terms <% "name")
    AND { "kind" IN @kind }?
    AND @composer { Some { EXISTS (SELECT 1 FROM "tune_composers" WHERE "tune_id" = "tune"."id" AND "composer_id" IN @composer) } | None { TRUE } }
ORDER BY "score" DESC, "name" ASC;

-- @get_extra_names_for
SELECT
    "tune_id",
    "extra_name"
FROM "tune_extra_names"
WHERE @tune_ids { One_of { "tune_id" IN @tune_ids } | All { TRUE } }
ORDER BY "extra_name";

-- @get_composers_for
SELECT
    "tune_id",
    "person"."id",
    "person"."name"
FROM "tune_composers"
JOIN "person" ON "tune_composers"."composer_id" = "person"."id"
WHERE @tune_ids { One_of { "tune_id" IN @tune_ids } | All { TRUE } }
ORDER BY "index";

-- @get_composers_with_details_for
SELECT
    "tune_id",
    "person"."id",
    "person"."name",
    "tune_composers"."details"
FROM "tune_composers"
JOIN "person" ON "tune_composers"."composer_id" = "person"."id"
WHERE @tune_ids { One_of { "tune_id" IN @tune_ids } | All { TRUE } }
ORDER BY "index";

-- @get_dances_for
SELECT
    "tune_id",
    "dance"."id",
    "dance"."name",
    "dance"."kind",
    "dance"."disambiguation"
FROM "recommended_tunes"
JOIN "dance" ON "recommended_tunes"."dance_id" = "dance"."id"
WHERE @tune_ids { One_of { "recommended_tunes"."tune_id" IN @tune_ids } | All { TRUE } };

-- @get_devisers_for_dances_of
SELECT
    "recommended_tunes"."dance_id",
    "person"."id",
    "person"."name"
FROM "recommended_tunes"
JOIN "dance_devisers" ON "recommended_tunes"."dance_id" = "dance_devisers"."dance_id"
JOIN "person" ON "dance_devisers"."deviser_id" = "person"."id"
WHERE @tune_ids { One_of { "recommended_tunes"."tune_id" IN @tune_ids } | All { TRUE } };

-- @get_versions_for
SELECT
    "id",
    "tune_id",
    "disambiguation",
    "monolithic_bars",
    "monolithic_or_default_structure"
FROM "version"
WHERE @tune_ids { One_of { "tune_id" IN @tune_ids } | All { TRUE } };

-- @get_sources_for_versions_of
SELECT
    "version_id",
    "source"."id",
    "source"."name",
    "source"."short_name"
FROM "version_sources"
JOIN "version" ON "version_sources"."version_id" = "version"."id"
JOIN "source" ON "version_sources"."source_id" = "source"."id"
WHERE @tune_ids { One_of { "version"."tune_id" IN @tune_ids } | All { TRUE } };

-- @get_arrangers_for_versions_of
SELECT
    "version_id",
    "person"."id",
    "person"."name"
FROM "version_arrangers"
JOIN "version" ON "version_arrangers"."version_id" = "version"."id"
JOIN "person" ON "version_arrangers"."arranger_id" = "person"."id"
WHERE @tune_ids { One_of { "version"."tune_id" IN @tune_ids } | All { TRUE } };

-- @get_rows_for_dance
SELECT
    "tune"."id",
    "name",
    "kind"
FROM "recommended_tunes"
JOIN "tune" ON "recommended_tunes"."tune_id" = "tune"."id"
WHERE "dance_id" = @dance_id;
