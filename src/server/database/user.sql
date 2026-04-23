-- @get
SELECT `yaml`
FROM `user`
WHERE `id` = @id;

-- @get_all
SELECT
    `id`,
    `yaml`
FROM `user`;

-- @update
INSERT INTO `user` (`id`, `yaml`)
VALUES (@id, @yaml)
ON DUPLICATE KEY UPDATE `yaml` = @yaml;

-- @delete
DELETE FROM `user`
WHERE `id` = @id;
