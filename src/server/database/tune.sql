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
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "kind",
    "remark",
    "scddb_id",
    "date",
    "created_at",
    "modified_at"
FROM "tune";

-- @create
INSERT INTO "tune" (
    "id",
    "name",
    "kind",
    "remark",
    "scddb_id",
    "date",
    "created_at",
    "modified_at"
) VALUES (
    @id,
    @name,
    @kind,
    @remark,
    @scddb_id,
    @date,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- @update
UPDATE "tune"
SET
    "name" = @name,
    "kind" = @kind,
    "remark" = @remark,
    "scddb_id" = @scddb_id,
    "date" = @date,
    "modified_at" = CURRENT_TIMESTAMP
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

-- @search
SELECT
    "search"."score",
    "tune"."id",
    "name",
    "kind"
FROM (
    SELECT
        "id",
	@needle {Some { word_similarity(@needle, "name") } | None { '1' } } AS "score"
    FROM "tune"
) AS "search"
JOIN "tune"
ON "tune"."id" = "search"."id"
WHERE "search"."score" >= @threshold
ORDER BY "score" DESC, "name" ASC;

-- @get_all_composers_new
SELECT
    "tune_id",
    "person"."id",
    "person"."name"
FROM "tune_composers"
JOIN "person"
ON "tune_composers"."composer_id" = "person"."id"
ORDER BY "index";
