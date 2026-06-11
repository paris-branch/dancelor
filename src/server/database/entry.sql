-- @get
SELECT "type" FROM "entry"
WHERE "id" = @id;

-- @register
INSERT INTO "entry" ("id", "type")
VALUES (@id, @type_);
