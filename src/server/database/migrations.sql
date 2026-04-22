-- -------------------------------------------------------------------------- --
-- Administrative stuff to support automated migrations
-- -------------------------------------------------------------------------- --

-- @create_table_migrations
CREATE TABLE IF NOT EXISTS `migrations` (
    `name` VARCHAR(255) PRIMARY KEY,
    `applied_at` TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- @get_migration
SELECT `applied_at` FROM `migrations`
WHERE `name` = @name;

-- @register_migration
INSERT INTO `migrations` (`name`) VALUES (@name);

-- -------------------------------------------------------------------------- --
-- First migrations: creation of basic id->yaml tables
-- -------------------------------------------------------------------------- --

-- @m001_2026_04_add_book_table
CREATE TABLE `book` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m002_2026_04_add_dance_table
CREATE TABLE `dance` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m003_2026_04_add_person_table
CREATE TABLE `person` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m004_2026_04_add_set_table
CREATE TABLE `set` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m005_2026_04_add_source_table
CREATE TABLE `source` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL,
    `cover` MEDIUMBLOB
);

-- @m006_2026_04_add_tune_table
CREATE TABLE `tune` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m007_2026_04_add_user_table
CREATE TABLE `user` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);

-- @m008_2026_04_add_version_table
CREATE TABLE `version` (
    `id` VARCHAR(14) PRIMARY KEY,
    `yaml` TEXT NOT NULL
);
