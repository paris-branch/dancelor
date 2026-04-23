-- @get
SELECT `yaml`
FROM `person`
WHERE `id` = @id;

-- @get_all
SELECT
    `id`,
    `yaml`
FROM `person`;

-- @update
INSERT INTO `person` (`id`, `yaml`)
VALUES (@id, @yaml)
ON DUPLICATE KEY UPDATE `yaml` = @yaml;

-- @delete
DELETE FROM `person`
WHERE `id` = @id;
