/*M!999999\- enable the sandbox mode */ 
-- MariaDB dump 10.19-11.4.9-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: dancelor
-- ------------------------------------------------------
-- Server version	11.4.9-MariaDB

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*M!100616 SET @OLD_NOTE_VERBOSITY=@@NOTE_VERBOSITY, NOTE_VERBOSITY=0 */;

--
-- Table structure for table `book`
--

DROP TABLE IF EXISTS `book`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `book` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `book`
--

LOCK TABLES `book` WRITE;
/*!40000 ALTER TABLE `book` DISABLE KEYS */;
INSERT INTO `book` VALUES
('0fi3-1iot-6tbq','value:\n  title: The Tam Lin Book\n  contents:\n    - - Versions\n      - - - xzzb-wasm-babe\n          - {}\n    - - Set\n      - ului-yd9x-o35w\n      - {}\nmeta:\n  created-at: \"2020-12-03T11:55:36+01:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  owners:\n    - lt3h-edgt-ac97\n  visibility:\n    - Everyone\n');
/*!40000 ALTER TABLE `book` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dance`
--

DROP TABLE IF EXISTS `dance`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `dance` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dance`
--

LOCK TABLES `dance` WRITE;
/*!40000 ALTER TABLE `dance` DISABLE KEYS */;
INSERT INTO `dance` VALUES
('l02q-i1j0-qpoi','value:\n  kind: 8 x 32 R\n  devisers:\n    - 8h62-3eis-xfem\n  names:\n    - The Architect\nmeta:\n  created-at: \"2023-12-21T17:11:33\"\n  modified-at: \"2023-12-21T17:11:33\"\naccess:\n  - Public\n');
/*!40000 ALTER TABLE `dance` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `migrations`
--

DROP TABLE IF EXISTS `migrations`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `migrations` (
  `name` varchar(255) NOT NULL,
  `applied_at` timestamp NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `migrations`
--

LOCK TABLES `migrations` WRITE;
/*!40000 ALTER TABLE `migrations` DISABLE KEYS */;
INSERT INTO `migrations` VALUES
('m001_2026_04_add_book_table','2026-04-22 12:52:21'),
('m002_2026_04_add_dance_table','2026-04-22 12:52:21'),
('m003_2026_04_add_person_table','2026-04-22 12:52:21'),
('m004_2026_04_add_set_table','2026-04-22 12:52:21'),
('m005_2026_04_add_source_table','2026-04-22 12:52:21'),
('m006_2026_04_add_tune_table','2026-04-22 12:52:21'),
('m007_2026_04_add_user_table','2026-04-22 12:52:21'),
('m008_2026_04_add_version_table','2026-04-22 12:52:21');
/*!40000 ALTER TABLE `migrations` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `person`
--

DROP TABLE IF EXISTS `person`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `person` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `person`
--

LOCK TABLES `person` WRITE;
/*!40000 ALTER TABLE `person` DISABLE KEYS */;
INSERT INTO `person` VALUES
('4plf-srss-ihav','value:\n  name: Davey Arthur\n  composed_tunes_are_public: true\nmeta:\n  created-at: \"2018-12-07T01:18:53+01:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  - Public\n'),
('8h62-3eis-xfem','value:\n  name: Mervyn C Short\n  scddb-id: 347\nmeta:\n  created-at: \"2023-07-03T14:17:45\"\n  modified-at: \"2023-07-03T14:17:45\"\naccess:\n  - Public\n'),
('uwoe-u6ij-ikgp','value:\n  name: Nicolas “Niols” Jeannerod\n  scddb-id: 11781\n  composed_tunes_are_public: true\n  user: lt3h-edgt-ac97\nmeta:\n  created-at: \"2018-10-12T11:50:54+02:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  - Public\n');
/*!40000 ALTER TABLE `person` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `set`
--

DROP TABLE IF EXISTS `set`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `set` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `set`
--

LOCK TABLES `set` WRITE;
/*!40000 ALTER TABLE `set` DISABLE KEYS */;
INSERT INTO `set` VALUES
('ului-yd9x-o35w','value:\n  name: Tam Lin Thrice\n  conceptors:\n    - uwoe-u6ij-ikgp\n  kind: 3x32R\n  versions-and-parameters:\n    - - xzzb-wasm-babe\n      - {}\n    - - xzzb-wasm-babe\n      - transposition: 2\n    - - xzzb-wasm-babe\n      - transposition: 7\n  order: 1,2,3\nmeta:\n  created-at: \"2023-05-02T11:16:55+00:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  owners:\n    - lt3h-edgt-ac97\n  visibility:\n    - Everyone\n'),
('wrwk-cz9g-g3wi','value:\n  name: A Private Set\n  conceptors:\n    - uwoe-u6ij-ikgp\n  kind: 3x32R\n  versions-and-parameters: []\n  order: \"1\"\nmeta:\n  created-at: \"2023-05-02T11:16:55+00:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  owners:\n    - lt3h-edgt-ac97\n  visibility:\n    - Owners_only\n');
/*!40000 ALTER TABLE `set` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `source`
--

DROP TABLE IF EXISTS `source`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `source` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  `cover` mediumblob DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `source`
--

LOCK TABLES `source` WRITE;
/*!40000 ALTER TABLE `source` DISABLE KEYS */;
INSERT INTO `source` VALUES
('2f8s-90v8-33do','value:\n  name: The Tam Lin Source\nmeta:\n  created-at: \"2025-04-12T18:45:27+00:00\"\n  modified-at: \"2025-04-12T18:45:27+00:00\"\naccess:\n  - Public\n',NULL);
/*!40000 ALTER TABLE `source` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tune`
--

DROP TABLE IF EXISTS `tune`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `tune` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tune`
--

LOCK TABLES `tune` WRITE;
/*!40000 ALTER TABLE `tune` DISABLE KEYS */;
INSERT INTO `tune` VALUES
('qdod-ad7l-8gr2','value:\n  names:\n    - Tam Lin\n  kind: R\n  composers:\n    - composer: 4plf-srss-ihav\n      details: \"\"\n  dances:\n    - l02q-i1j0-qpoi\nmeta:\n  created-at: \"2018-12-07T01:18:53+01:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  - Public\n');
/*!40000 ALTER TABLE `tune` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `user` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES
('lt3h-edgt-ac97','value:\n  password: $argon2id$v=19$m=65536,t=2,p=1$mm4GoaR1lz2r6jJf2OomVA$VwSQPpYI6Clwh8xdoOBcwX2BFH8VCv3B++Tx1G5B11w\n  username: Niols\nmeta:\n  created-at: \"2025-04-13T18:48:00+02:00\"\n  modified-at: \"2025-04-13T18:48:00+02:00\"\naccess:\n  - Public\n');
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `version`
--

DROP TABLE IF EXISTS `version`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8mb4 */;
CREATE TABLE `version` (
  `id` varchar(14) NOT NULL,
  `yaml` text NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `version`
--

LOCK TABLES `version` WRITE;
/*!40000 ALTER TABLE `version` DISABLE KEYS */;
INSERT INTO `version` VALUES
('xzzb-wasm-babe','value:\n  tune: qdod-ad7l-8gr2\n  key: Dm\n  disambiguation: Niols\'s Version\n  arrangers:\n    - uwoe-u6ij-ikgp\n  sources:\n    - source: 2f8s-90v8-33do\n      structure: AABB\n  content:\n    - Monolithic\n    - bars: 32\n      structure: AABB\n      lilypond:\n        \"\\\\relative c\' <<\\n  {\\n    \\\\clef treble\\n    \\\\key d \\\\minor\\n \\\n        \\   \\\\time 4/4\\n\\n    \\\\repeat volta 2 {\\n      \\\\partial 8 r8 |\\n      a4\\\n        \\ d8 a f\' a, d a |\\n      bes4 d8 bes f\' bes, d bes |\\n      c4 e8 c g\' c,\\\n        \\ e g |\\n      f8 e d c d c a g |\\n      \\\\break\\n\\n      a4 d8 a f\' a, d\\\n        \\ a |\\n      bes4 d8 bes f\' bes, d bes |\\n      c4 e8 c g\' c, e g |\\n    \\\n        \\  f8 e d c d4.\\n    } \\\\break\\n\\n    \\\\repeat volta 2 {\\n      a\'8 |\\n  \\\n        \\    d8 a a a f a d, a\' |\\n      d8 a a a f a d, a\' |\\n      c8 g g g c g\\\n        \\ e\' g, |\\n      c8 g g g c[ r c cis] |\\n      \\\\break\\n\\n      d8 a a a f\\\n        \\ a d, a\' |\\n      d8 a a a f[ a] d, r |\\n      bes8 a bes c d c d e |\\n \\\n        \\     f8 e d c a[ d] d\\n    }\\n  }\\n\\n  \\\\new ChordNames {\\n    \\\\chordmode\\\n        \\ {\\n      s8 |\\n      d1:m | bes | c | d2:m a:m |\\n      d1:m | bes | c |\\\n        \\ a2:m d4.:m\\n\\n      s8 |\\n      d1:m | s | c | s |\\n      d1:m | s | g:m\\\n        \\ | a2:m d4.:m\\n    }\\n  }\\n\\n  \\\\new ChordNames {\\n    \\\\chordmode {\\n  \\\n        \\    s8 |\\n      s1 | \\\\parenthesize g:m | s | s |\\n      s1 | \\\\parenthesize\\\n        \\ g:m | s | s2 s4.\\n\\n      s8 |\\n      s1 | s | s | s |\\n      \\\\parenthesize\\\n        \\ bes1 | s | s | s2 s4.\\n    }\\n  }\\n>>\\n\"\nmeta:\n  created-at: \"2023-04-04T18:45:27+00:00\"\n  modified-at: \"2023-06-25T16:51:15+02:00\"\naccess:\n  - Public\n');
/*!40000 ALTER TABLE `version` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*M!100616 SET NOTE_VERBOSITY=@OLD_NOTE_VERBOSITY */;

-- Dump completed
