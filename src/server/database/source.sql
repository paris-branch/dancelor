-- @get
SELECT `yaml`
FROM `source`
WHERE `id` = @id;

-- @get_all
SELECT
    `id`,
    `yaml`
FROM `source`;

-- @update
INSERT INTO `source` (`id`, `yaml`)
VALUES (@id, @yaml)
ON DUPLICATE KEY UPDATE `yaml` = @yaml;

-- @delete
DELETE FROM `source`
WHERE `id` = @id;

-- @get_cover
SELECT `cover`
FROM `source`
WHERE `id` = @id;
