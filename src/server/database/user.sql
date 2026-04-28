-- @get
SELECT
    "username",
    "password",
    "password_reset_token_hash",
    "password_reset_token_max_date",
    "role",
    "omniscience",
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
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user";

-- @create
INSERT INTO "user" (
    "id",
    "username",
    "password_reset_token_hash",
    "password_reset_token_max_date",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
)
VALUES (
    @id,
    @username,
    @password_reset_token_hash,
    @password_reset_token_max_date,
    @role,
    @omniscience,
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
    "role" = @role,
    "omniscience" = @omniscience,
    "modified_at" = CURRENT_TIMESTAMP
WHERE "id" = @id;

-- @delete
DELETE FROM "user"
WHERE "id" = @id;

-- @set_password_reset_token
UPDATE "user"
SET
    "password" = NULL,
    "password_reset_token_hash" = @password_reset_token_hash,
    "password_reset_token_max_date" = @password_reset_token_max_date
WHERE "id" = @id;

-- @remove_all_remember_me_tokens
DELETE FROM "remember_me_tokens"
WHERE "user_id" = @user_id;

-- @remove_one_remember_me_token
DELETE FROM "remember_me_tokens"
WHERE "user_id" = @user_id AND "key" = @key;

-- @find_remember_me_token
SELECT
    "hash",
    "max_date"
FROM "remember_me_tokens"
WHERE "user_id" = @user_id AND "key" = @key;

-- @add_remember_me_token
INSERT INTO "remember_me_tokens" (
    "user_id",
    "key",
    "hash",
    "max_date"
)
VALUES (
    @user_id,
    @key,
    @hash,
    @max_date
);

-- @set_password
UPDATE "user"
SET
    "password" = @password,
    "password_reset_token_hash" = NULL,
    "password_reset_token_max_date" = NULL
WHERE "id" = @id;

-- @set_omniscience
UPDATE "user"
SET
    "omniscience" = @omniscience
WHERE "id" = @id;
