-- @get_type
SELECT "type" FROM "entry"
WHERE "id" = @id;

-- @get_visibility
SELECT "visibility" FROM "entry"
WHERE "id" = @id;

-- @register
INSERT INTO "entry" (
    "id",
    "type",
    "created_at",
    "modified_at",
    "visibility"
) VALUES (
    @id,
    @type_,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP,
    @visibility
);

-- @delete
DELETE FROM "entry" WHERE "id" = @id;

-- @touch
UPDATE "entry"
SET "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;

-- @update_visibility
UPDATE "entry"
SET "visibility" = @visibility
WHERE "id" = @id;

-- @get_viewers
SELECT "viewer_id"
FROM "entry_viewers"
WHERE "entry_id" = @entry_id;

-- @get_all_viewers
SELECT
    "entry_id",
    "viewer_id"
FROM "entry_viewers"
JOIN "entry" ON "entry_viewers"."entry_id" = "entry"."id"
WHERE "type" = @type_;

-- @delete_all_viewers
DELETE FROM "entry_viewers"
WHERE "entry_id" = @entry_id;

-- @add_one_viewer
INSERT INTO "entry_viewers" (
    "entry_id",
    "viewer_id"
) VALUES (
    @entry_id,
    @viewer_id
);

-- @get_owners
SELECT "owner_id"
FROM "entry_owners"
WHERE "entry_id" = @entry_id;

-- @get_all_owners
SELECT
    "entry_id",
    "owner_id"
FROM "entry_owners"
JOIN "entry" ON "entry_owners"."entry_id" = "entry"."id"
WHERE "type" = @type_;

-- @delete_all_owners
DELETE FROM "entry_owners"
WHERE "entry_id" = @entry_id;

-- @add_one_owner
INSERT INTO "entry_owners" (
    "entry_id",
    "owner_id"
) VALUES (
    @entry_id,
    @owner_id
);

-- @get_newest
SELECT
    "entry"."id",
    "entry"."type"
FROM "entry"
LEFT JOIN "entry_owners" ON "entry_owners"."entry_id" = "entry"."id" AND "entry_owners"."owner_id" = (@user_id :: TEXT NULL)
LEFT JOIN "entry_viewers" ON "entry_viewers"."entry_id" = "entry"."id" AND "entry_viewers"."viewer_id" = (@user_id :: TEXT NULL)
LEFT JOIN "user" ON "user"."id" = (@user_id :: TEXT NULL)
WHERE
    ("entry"."type" IN ('Person', 'Dance', 'Source', 'Tune', 'Version', 'User'))
    OR ("entry"."visibility" = 'Everyone')
    OR ("entry_owners"."owner_id" IS NOT NULL)
    OR ("entry"."visibility" = 'Select_viewers' AND "entry_viewers"."viewer_id" IS NOT NULL)
    OR ("user"."role" = 'Administrator' AND "user"."omniscience")
ORDER BY "created_at" DESC
LIMIT @limit;
