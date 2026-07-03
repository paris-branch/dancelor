-- @get
SELECT
    "tune_id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
FROM "version"
JOIN "entry" ON "version"."id" = "entry"."id"
WHERE "version"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @get_all
SELECT
    "version"."id",
    "tune_id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
FROM "version"
JOIN "entry" ON "version"."id" = "entry"."id";

-- @get_all_for_tune
SELECT
    "version"."id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
FROM "version"
JOIN "entry" ON "version"."id" = "entry"."id"
WHERE "tune_id" = @tune_id;

-- @create
INSERT INTO "version" (
    "id",
    "tune_id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure"
) VALUES (
    @id,
    @tune_id,
    @key,
    @remark,
    @disambiguation,
    @monolithic_lilypond,
    @monolithic_bars,
    @monolithic_or_default_structure
);

-- @update
UPDATE "version"
SET
    "tune_id" = @tune_id,
    "key" = @key,
    "remark" = @remark,
    "disambiguation" = @disambiguation,
    "monolithic_lilypond" = @monolithic_lilypond,
    "monolithic_bars" = @monolithic_bars,
    "monolithic_or_default_structure" = @monolithic_or_default_structure
WHERE "id" = @id;

-- @delete
DELETE FROM "version"
WHERE "id" = @id;

-- @get_arrangers
SELECT "arranger_id"
FROM "version_arrangers"
WHERE "version_id" = @version_id;

-- @get_all_arrangers
SELECT
    "version_id",
    "arranger_id"
FROM "version_arrangers";

-- @delete_all_arrangers
DELETE FROM "version_arrangers"
WHERE "version_id" = @version_id;

-- @add_one_arranger
INSERT INTO "version_arrangers" (
    "version_id",
    "arranger_id"
) VALUES (
    @version_id,
    @arranger_id
);

-- @get_sources
SELECT
    "source_id",
    "structure",
    "details"
FROM "version_sources"
WHERE "version_id" = @version_id;

-- @get_all_sources
SELECT
    "version_id",
    "source_id",
    "structure",
    "details"
FROM "version_sources";

-- @delete_all_sources
DELETE FROM "version_sources"
WHERE "version_id" = @version_id;

-- @add_one_source
INSERT INTO "version_sources" (
    "version_id",
    "source_id",
    "structure",
    "details"
) VALUES (
    @version_id,
    @source_id,
    @structure,
    @details
);

-- @get_destructured_parts
SELECT
    "part",
    "melody",
    "chords"
FROM "version_destructured_parts"
WHERE "version_id" = @version_id
ORDER BY "part";

-- @get_all_destructured_parts
SELECT
    "version_id",
    "part",
    "melody",
    "chords"
FROM "version_destructured_parts"
ORDER BY "version_id", "part";

-- @delete_all_destructured_parts
DELETE FROM "version_destructured_parts"
WHERE "version_id" = @version_id;

-- @add_one_destructured_part
INSERT INTO "version_destructured_parts" (
    "version_id",
    "part",
    "melody",
    "chords"
) VALUES (
    @version_id,
    @part,
    @melody,
    @chords
);

-- @get_destructured_transitions
SELECT
    "from_parts",
    "to_parts",
    "melody",
    "chords"
FROM "version_destructured_transitions"
WHERE "version_id" = @version_id
ORDER BY "from_parts", "to_parts";

-- @get_all_destructured_transitions
SELECT
    "version_id",
    "from_parts",
    "to_parts",
    "melody",
    "chords"
FROM "version_destructured_transitions"
ORDER BY "version_id", "from_parts", "to_parts";

-- @delete_all_destructured_transitions
DELETE FROM "version_destructured_transitions"
WHERE "version_id" = @version_id;

-- @add_one_destructured_transition
INSERT INTO "version_destructured_transitions" (
    "version_id",
    "from_parts",
    "to_parts",
    "melody",
    "chords"
) VALUES (
    @version_id,
    @from_parts,
    @to_parts,
    @melody,
    @chords
);

-- NEW MODELS

-- @get_rows
WITH "versions" AS &get_version_and_tune_rows
SELECT *
FROM "versions"
WHERE "id" IN @ids;

-- @get_view
WITH "versions" AS &get_version_views
SELECT "versions".*
FROM "versions"
JOIN "tune" ON "versions"."tune_id" = "tune"."id"
WHERE "versions"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @search
WITH "version_rows" AS &get_version_and_tune_rows
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "tune_name") END AS "score",
    "version_rows".*
FROM "version_rows"
JOIN "version" ON "version_rows"."id" = "version"."id"
WHERE
    (@terms = '' OR @terms <% "tune_name")
    AND { "key" IN @key }?
    AND @source { Some { EXISTS (SELECT 1 FROM "version_sources" WHERE "version_id" = "version"."id" AND "source_id" IN @source) } | None { TRUE } }
    AND { "tune_kind" IN @tune_kind }?
    AND @tune_composer { Some { EXISTS (SELECT 1 FROM "tune_composers" WHERE "tune_id" = "version"."tune_id" AND "composer_id" IN @tune_composer) } | None { TRUE } }
ORDER BY "score" DESC, "tune_name" ASC;

-- @get_arrangers_for
WITH "persons" AS &get_person_rows
SELECT
    "version_id",
    "persons".*
FROM "version_arrangers"
JOIN "persons" ON "version_arrangers"."arranger_id" = "persons"."id"
WHERE @version_ids { One_of { "version_id" IN @version_ids } | All { TRUE } };

-- @get_sources_for
WITH "sources" AS &get_source_short_names
SELECT
    "version_id",
    "sources".*
FROM "version_sources"
JOIN "sources" ON "version_sources"."source_id" = "sources"."id"
WHERE @version_ids { One_of { "version_id" IN @version_ids } | All { TRUE } };

-- @get_version_sources_for
WITH "sources" AS &get_source_names
SELECT
    "version_id",
    "sources".*,
    "structure",
    "details"
FROM "version_sources"
JOIN "sources" ON "version_sources"."source_id" = "sources"."id"
WHERE @version_ids { One_of { "version_id" IN @version_ids } | All { TRUE } };
