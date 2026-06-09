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
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "tune_id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
FROM "version";

-- @get_all_for_tune
SELECT
    "id",
    "key",
    "remark",
    "disambiguation",
    "monolithic_lilypond",
    "monolithic_bars",
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
FROM "version"
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
    "monolithic_or_default_structure",
    "created_at",
    "modified_at"
) VALUES (
    @id,
    @tune_id,
    @key,
    @remark,
    @disambiguation,
    @monolithic_lilypond,
    @monolithic_bars,
    @monolithic_or_default_structure,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
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
    "monolithic_or_default_structure" = @monolithic_or_default_structure,
    "modified_at" = CURRENT_TIMESTAMP
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

-- @search
SELECT
    "search"."score",
    "version"."id",
    "tune"."id" AS "tune_id",
    -- version
    "version"."disambiguation",
    "version"."monolithic_bars",
    "version"."monolithic_or_default_structure",
    -- tune
    "name" AS "tune_name",
    "kind" AS "tune_kind"
FROM (
    SELECT
        "id",
        CASE WHEN @needle = '' THEN 1.0
             ELSE word_similarity(@needle, "name")
        END AS "score"
    FROM "tune"
) AS "search"
JOIN "tune" ON "tune"."id" = "search"."id"
JOIN "version" ON "version"."tune_id" = "tune"."id"
WHERE "search"."score" >= @threshold
ORDER BY "score" DESC, "name" ASC;

-- @get_all_arrangers_new
SELECT
    "version_id",
    "person"."id",
    "person"."name"
FROM "version_arrangers"
JOIN "person" ON "version_arrangers"."arranger_id" = "person"."id";

-- @get_all_sources_new
SELECT
    "version_id",
    "source"."id",
    "source"."name",
    "source"."short_name"
FROM "version_sources"
JOIN "source" ON "version_sources"."source_id" = "source"."id";
