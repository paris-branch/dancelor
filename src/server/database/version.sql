-- @get
SELECT `yaml`
FROM `version`
WHERE `id` = @id;

-- @get_all
SELECT
    `id`,
    `yaml`
FROM `version`;

-- @update
INSERT INTO `version` (`id`, `yaml`)
VALUES (@id, @yaml)
ON DUPLICATE KEY UPDATE `yaml` = @yaml;

-- @delete
DELETE FROM `version`
WHERE `id` = @id;
