 

CREATE TABLE `book` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_book_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `dance` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_dance_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `globally_unique_id` (
  `id` varchar(14) NOT NULL,
  `type` enum('Book','Dance','Person','Set','Source','Tune','User','Version') NOT NULL,
  PRIMARY KEY (`id`)
);

CREATE TABLE `person` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_person_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `set` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_set_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `source` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  `cover` mediumblob DEFAULT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_source_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `tune` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_tune_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `user` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_user_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);

CREATE TABLE `version` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_version_id` FOREIGN KEY (`id`) REFERENCES `globally_unique_id` (`id`)
);
