-- @get
SELECT
    "username",
    "password",
    "password_reset_token_hash",
    "password_reset_token_max_date",
    "remember_me_tokens",
    "role",
    "created_at",
    "modified_at"
FROM "user"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "username",
    "password",
    "password_reset_token_hash",
    "password_reset_token_max_date",
    "remember_me_tokens",
    "role",
    "created_at",
    "modified_at"
FROM "user";

-- @insert
INSERT INTO "user" (
    "id",
    "username",
    "password",
    "password_reset_token_hash",
    "password_reset_token_max_date",
    "remember_me_tokens",
    "role",
    "created_at",
    "modified_at"
)
VALUES (
    @id,
    @username,
    @password,
    @password_reset_token_hash,
    @password_reset_token_max_date,
    @remember_me_tokens,
    @role,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- @update
UPDATE "user"
SET
    "username" = @username,
    "password" = @password,
    "password_reset_token_hash" = @password_reset_token_hash,
    "password_reset_token_max_date" = @password_reset_token_max_date,
    "remember_me_tokens" = @remember_me_tokens,
    "role" = @role,
    "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;

-- @delete
DELETE FROM "user"
WHERE "id" = @id;
