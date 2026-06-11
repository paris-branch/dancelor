-- @get
SELECT "type" FROM "entry"
WHERE "id" = @id;

-- @register
INSERT INTO "entry" (
    "id",
    "type",
    "created_at",
    "modified_at"
) VALUES (
    @id,
    @type_,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- @touch
UPDATE "entry"
SET "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;
