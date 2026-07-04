-- @get
SELECT
    "username",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user"
JOIN "entry" ON "user"."id" = "entry"."id"
WHERE "user"."id" = @id
LIMIT 1; -- NOTE: to help sqlgg

-- @get_all
SELECT
    "entry"."id",
    "username",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user"
JOIN "entry" ON "user"."id" = "entry"."id";

-- @get_from_username
SELECT
    "user"."id",
    "role",
    "omniscience",
    "created_at",
    "modified_at"
FROM "user"
JOIN "entry" ON "user"."id" = "entry"."id"
WHERE "username" = @username
LIMIT 1; -- NOTE: to help sqlgg

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
    "omniscience"
)
VALUES (
    @id,
    @username,
    @password_reset_token_hash,
    @password_reset_token_max_date,
    @role,
    @omniscience
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

-- @get_rows
WITH "users" AS &get_user_rows
SELECT *
FROM "users"
WHERE "id" IN @ids;

-- @search
WITH "users" AS &get_user_rows
SELECT
    CASE WHEN @terms = '' THEN 1.0 ELSE word_similarity(@terms, "username") END AS "score",
    "users".*
FROM "users"
WHERE (@terms = '' OR @terms <% "username")
ORDER BY "score" DESC, "username" ASC;
