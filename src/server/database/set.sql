-- @get
SELECT
    "name",
    "kind",
    "order",
    "instructions",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
FROM "set"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "name",
    "kind",
    "order",
    "instructions",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
FROM "set";

-- @create
INSERT INTO "set" (
    "id",
    "name",
    "kind",
    "order",
    "instructions",
    "remark",
    "created_at",
    "modified_at",
    "visibility"
) VALUES (
    @id,
    @name,
    @kind,
    @order,
    @instructions,
    @remark,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP,
    @visibility
);

-- @update
UPDATE "set"
SET
    "name" = @name,
    "kind" = @kind,
    "order" = @order,
    "instructions" = @instructions,
    "remark" = @remark,
    "modified_at" = CURRENT_TIMESTAMP,
    "visibility" = @visibility
WHERE "id" = @id;

-- @delete
DELETE FROM "set"
WHERE "id" = @id;

-- @get_conceptors
SELECT "conceptor_id"
FROM "set_conceptors"
WHERE "set_id" = @set_id;

-- @get_all_conceptors
SELECT
    "set_id",
    "conceptor_id"
FROM "set_conceptors";

-- @delete_all_conceptors
DELETE FROM "set_conceptors"
WHERE "set_id" = @set_id;

-- @add_one_conceptor
INSERT INTO "set_conceptors" (
    "set_id",
    "conceptor_id"
) VALUES (
    @set_id,
    @conceptor_id
);

-- @get_dances
SELECT "dance_id"
FROM "set_dances"
WHERE "set_id" = @set_id;

-- @get_all_dances
SELECT
    "set_id",
    "dance_id"
FROM "set_dances";

-- @delete_all_dances
DELETE FROM "set_dances"
WHERE "set_id" = @set_id;

-- @add_one_dance
INSERT INTO "set_dances" (
    "set_id",
    "dance_id"
) VALUES (
    @set_id,
    @dance_id
);

-- @get_viewers
SELECT "viewer_id"
FROM "set_viewers"
WHERE "set_id" = @set_id;

-- @get_all_viewers
SELECT
    "set_id",
    "viewer_id"
FROM "set_viewers";

-- @delete_all_viewers
DELETE FROM "set_viewers"
WHERE "set_id" = @set_id;

-- @add_one_viewer
INSERT INTO "set_viewers" (
    "set_id",
    "viewer_id"
) VALUES (
    @set_id,
    @viewer_id
);

-- @get_owners
SELECT "owner_id"
FROM "set_owners"
WHERE "set_id" = @set_id;

-- @get_all_owners
SELECT
    "set_id",
    "owner_id"
FROM "set_owners";

-- @delete_all_owners
DELETE FROM "set_owners"
WHERE "set_id" = @set_id;

-- @add_one_owner
INSERT INTO "set_owners" (
    "set_id",
    "owner_id"
) VALUES (
    @set_id,
    @owner_id
);

-- @get_content
SELECT
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "set_content"
WHERE "set_id" = @set_id
ORDER BY "index";

-- @get_all_content
SELECT
    "set_id",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
FROM "set_content"
ORDER BY "set_id", "index";

-- @delete_all_content
DELETE FROM "set_content"
WHERE "set_id" = @set_id;

-- @add_one_content_item
INSERT INTO "set_content" (
    "set_id",
    "index",
    "version_id",
    "version_parameter_transposition_semitones",
    "version_parameter_first_bar",
    "version_parameter_clef",
    "version_parameter_structure",
    "version_parameter_trivia",
    "version_parameter_display_name",
    "version_parameter_display_composer"
) VALUES (
    @set_id,
    @index,
    @version_id,
    @version_parameter_transposition_semitones,
    @version_parameter_first_bar,
    @version_parameter_clef,
    @version_parameter_structure,
    @version_parameter_trivia,
    @version_parameter_display_name,
    @version_parameter_display_composer
);
