CREATE TABLE `book` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `dance` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `migrations` (
    `name` varchar(255) NOT NULL,
    `applied_at` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP (),
    PRIMARY KEY (`name`)
);

CREATE TABLE `person` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `set` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `source` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    `cover` mediumblob DEFAULT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `tune` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `user` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE `version` (
    `id` varchar(14) NOT NULL,
    `yaml` text NOT NULL,
    PRIMARY KEY (`id`)
);
