-- @get
SELECT
    "username",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user"
WHERE "id" = @id;

-- @get_person
SELECT
    "person_id"
FROM "user"
WHERE "id" = @id;

-- @get_all
SELECT
    "id",
    "username",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user";

-- @get_from_username
SELECT
    "id",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user"
WHERE "username" = @username;

-- @get_password_from_username
SELECT
    "password"
FROM "user"
WHERE "username" = @username;

-- @get_password_reset_token_from_username
SELECT
    "password_reset_token_hash",
    "password_reset_token_max_date"
FROM "user"
WHERE "username" = @username;

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
