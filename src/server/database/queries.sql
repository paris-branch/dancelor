-- @list_books
SELECT
    `id`,
    `yaml`
FROM `book`;

-- @list_dances
SELECT
    `id`,
    `yaml`
FROM `dance`;

-- @list_persons
SELECT
    `id`,
    `yaml`
FROM `person`;

-- @list_sets
SELECT
    `id`,
    `yaml`
FROM `set`;

-- @list_sources
SELECT
    `id`,
    `yaml`
FROM `source`;

-- @get_source_cover
SELECT `cover` FROM `source`
WHERE `id` = @id;

-- @list_tunes
SELECT
    `id`,
    `yaml`
FROM `tune`;

-- @list_users
SELECT
    `id`,
    `yaml`
FROM `user`;

-- @list_versions
SELECT
    `id`,
    `yaml`
FROM `version`;

-- @update_book
INSERT INTO `book` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_dance
INSERT INTO `dance` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_person
INSERT INTO `person` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_set
INSERT INTO `set` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_source
INSERT INTO `source` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_tune
INSERT INTO `tune` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_user
INSERT INTO `user` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @update_version
INSERT INTO `version` (`id`, `yaml`) VALUES (
    @id, @yaml
) ON DUPLICATE KEY UPDATE `yaml`
= @yaml;

-- @delete_book
DELETE FROM `book`
WHERE `id` = @id;

-- @delete_dance
DELETE FROM `dance`
WHERE `id` = @id;

-- @delete_person
DELETE FROM `person`
WHERE `id` = @id;

-- @delete_set
DELETE FROM `set`
WHERE `id` = @id;

-- @delete_source
DELETE FROM `source`
WHERE `id` = @id;

-- @delete_tune
DELETE FROM `tune`
WHERE `id` = @id;

-- @delete_user
DELETE FROM `user`
WHERE `id` = @id;

-- @delete_version
DELETE FROM `version`
WHERE `id` = @id;
