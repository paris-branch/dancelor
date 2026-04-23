-- @get
SELECT `yaml`
FROM `tune`
WHERE `id` = @id;

-- @get_all
SELECT
    `id`,
    `yaml`
FROM `tune`;

-- @update
INSERT INTO `tune` (`id`, `yaml`)
VALUES (@id, @yaml)
ON DUPLICATE KEY UPDATE `yaml` = @yaml;

-- @delete
DELETE FROM `tune`
WHERE `id` = @id;
