--
-- PostgreSQL database dump
--

\restrict ybiQOqa6jiLe8LvxFznU4q8n34iwf9VXgUBS2e7j5NMdWOEHU3ms2YSbcIZ871W

-- Dumped from database version 17.9
-- Dumped by pg_dump version 17.9

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: dancelor; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA "dancelor";


--
-- Name: globally_unique_id_type; Type: TYPE; Schema: dancelor; Owner: -
--

CREATE TYPE "dancelor"."globally_unique_id_type" AS ENUM (
    'Book',
    'Dance',
    'Person',
    'Set',
    'Source',
    'Tune',
    'User',
    'Version'
);


--
-- Name: book; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."book" (
    "id" character varying(14) NOT NULL,
    "json" json NOT NULL
);


--
-- Name: dance; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."dance" (
    "id" character varying(14) NOT NULL,
    "name" character varying(256) NOT NULL,
    "kind" character varying(32) NOT NULL,
    "two_chords" smallint NOT NULL,
    "scddb_id" integer,
    "disambiguation" character varying(256) NOT NULL,
    "date" character varying(32),
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL
);


--
-- Name: dance_devisers; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."dance_devisers" (
    "dance_id" character varying(14) NOT NULL,
    "index" integer NOT NULL,
    "deviser_id" character varying(14) NOT NULL
);


--
-- Name: dance_extra_names; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."dance_extra_names" (
    "dance_id" character varying(14) NOT NULL,
    "extra_name" character varying(256) NOT NULL
);


--
-- Name: globally_unique_id; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."globally_unique_id" (
    "id" character varying(14) NOT NULL,
    "type" "dancelor"."globally_unique_id_type" NOT NULL
);


--
-- Name: migrations; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."migrations" (
    "name" character varying(255) NOT NULL,
    "applied_at" timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: person; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."person" (
    "id" character varying(14) NOT NULL,
    "name" character varying(256) NOT NULL,
    "scddb_id" integer,
    "composed_tunes_are_public" boolean NOT NULL,
    "published_tunes_are_public" boolean NOT NULL,
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL
);


--
-- Name: recommended_tunes; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."recommended_tunes" (
    "dance_id" character varying(14) NOT NULL,
    "tune_id" character varying(14) NOT NULL
);


--
-- Name: remember_me_tokens; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."remember_me_tokens" (
    "user_id" character varying(14) NOT NULL,
    "key" character varying(256) NOT NULL,
    "hash" character varying(256) NOT NULL,
    "max_date" timestamp without time zone NOT NULL
);


--
-- Name: set; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."set" (
    "id" character varying(14) NOT NULL,
    "json" json NOT NULL
);


--
-- Name: source; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."source" (
    "id" character varying(14) NOT NULL,
    "cover" "bytea",
    "name" character varying(256) NOT NULL,
    "short_name" character varying(64),
    "scddb_id" integer,
    "description" "text",
    "date" character varying(32),
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL
);


--
-- Name: source_editors; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."source_editors" (
    "source_id" character varying(14) NOT NULL,
    "person_id" character varying(14) NOT NULL
);


--
-- Name: tune; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."tune" (
    "id" character varying(14) NOT NULL,
    "name" character varying(256) NOT NULL,
    "kind" character varying(32) NOT NULL,
    "remark" character varying(256) NOT NULL,
    "scddb_id" integer,
    "date" character varying(32),
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL
);


--
-- Name: tune_composers; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."tune_composers" (
    "tune_id" character varying(14) NOT NULL,
    "index" integer NOT NULL,
    "composer_id" character varying(14) NOT NULL,
    "details" character varying(256) NOT NULL
);


--
-- Name: tune_extra_names; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."tune_extra_names" (
    "tune_id" character varying(14) NOT NULL,
    "extra_name" character varying(256) NOT NULL
);


--
-- Name: user; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."user" (
    "id" character varying(14) NOT NULL,
    "username" character varying(256) NOT NULL,
    "password" character varying(256),
    "password_reset_token_hash" character varying(256),
    "password_reset_token_max_date" timestamp without time zone,
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL,
    "role" smallint NOT NULL,
    "omniscience" boolean NOT NULL,
    "person_id" character varying(14)
);


--
-- Name: version; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."version" (
    "id" character varying(14) NOT NULL,
    "json" json NOT NULL
);


--
-- Data for Name: book; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."book" ("id", "json") VALUES ('0fi3-1iot-6tbq', '{"value":{"title":"The Tam Lin Book","contents":[["Versions",[["xzzb-wasm-babe",{}]]],["Set","ului-yd9x-o35w",{}]]},"meta":{"created-at":"2020-12-03T11:55:36+01:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":{"owners":["lt3h-edgt-ac97"],"visibility":["Everyone"]}}');


--
-- Data for Name: dance; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."dance" ("id", "name", "kind", "two_chords", "scddb_id", "disambiguation", "date", "created_at", "modified_at") VALUES ('l02q-i1j0-qpoi', 'The Architect', '8 x 32 R', 0, NULL, '', NULL, '2023-12-21 18:11:33', '2023-12-21 18:11:33');
INSERT INTO "dancelor"."dance" ("id", "name", "kind", "two_chords", "scddb_id", "disambiguation", "date", "created_at", "modified_at") VALUES ('cy5n-qvpl-k0yl', 'Test dance', '8 x 32 R', 2, 1234, 'sdlfkj', '2188', '2026-05-09 14:11:26', '2026-05-09 14:11:26');
INSERT INTO "dancelor"."dance" ("id", "name", "kind", "two_chords", "scddb_id", "disambiguation", "date", "created_at", "modified_at") VALUES ('0xf7-xwz9-1fhj', 'Test dance A439', '16 x 32 S', 2, 123199, '', '3088', '2026-05-10 11:36:21.52889', '2026-05-10 11:36:21.52889');


--
-- Data for Name: dance_devisers; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('l02q-i1j0-qpoi', 0, '8h62-3eis-xfem');
INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('cy5n-qvpl-k0yl', 0, 'uwoe-u6ij-ikgp');
INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('cy5n-qvpl-k0yl', 1, '9fdg-glrm-0zoi');
INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('cy5n-qvpl-k0yl', 2, '8h62-3eis-xfem');
INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('0xf7-xwz9-1fhj', 0, 'uwoe-u6ij-ikgp');
INSERT INTO "dancelor"."dance_devisers" ("dance_id", "index", "deviser_id") VALUES ('0xf7-xwz9-1fhj', 1, '9fdg-glrm-0zoi');


--
-- Data for Name: dance_extra_names; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."dance_extra_names" ("dance_id", "extra_name") VALUES ('cy5n-qvpl-k0yl', 'The test dance');
INSERT INTO "dancelor"."dance_extra_names" ("dance_id", "extra_name") VALUES ('cy5n-qvpl-k0yl', 'A test dance');
INSERT INTO "dancelor"."dance_extra_names" ("dance_id", "extra_name") VALUES ('cy5n-qvpl-k0yl', 'A test dance with many names');


--
-- Data for Name: globally_unique_id; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('0fi3-1iot-6tbq', 'Book');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('2f8s-90v8-33do', 'Source');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('4plf-srss-ihav', 'Person');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('8h62-3eis-xfem', 'Person');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('l02q-i1j0-qpoi', 'Dance');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('lt3h-edgt-ac97', 'User');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('qdod-ad7l-8gr2', 'Tune');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('ului-yd9x-o35w', 'Set');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('uwoe-u6ij-ikgp', 'Person');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('wrwk-cz9g-g3wi', 'Set');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('xzzb-wasm-babe', 'Version');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('9fdg-glrm-0zoi', 'Person');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('cy5n-qvpl-k0yl', 'Dance');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('0xf7-xwz9-1fhj', 'Dance');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('rifw-ul36-3uq5', 'Tune');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('xsbz-vqy7-xj3s', 'Version');
INSERT INTO "dancelor"."globally_unique_id" ("id", "type") VALUES ('2wrv-25yu-yc07', 'Source');


--
-- Data for Name: migrations; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m001_2026_04_add_book_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m002_2026_04_add_dance_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m003_2026_04_add_person_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m004_2026_04_add_set_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m005_2026_04_add_source_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m006_2026_04_add_tune_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m007_2026_04_add_user_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m008_2026_04_add_version_table', '2026-04-22 14:52:21+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m009_2026_04_add_globally_unique_id_table', '2026-04-23 23:38:30+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m010_2026_04_insert_ids_from_book_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m011_2026_04_add_fk_book_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m012_2026_04_insert_ids_from_dance_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m013_2026_04_add_fk_dance_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m014_2026_04_insert_ids_from_person_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m015_2026_04_add_fk_person_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m016_2026_04_insert_ids_from_set_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m017_2026_04_add_fk_set_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m018_2026_04_insert_ids_from_source_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m019_2026_04_add_fk_source_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m020_2026_04_insert_ids_from_tune_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m021_2026_04_add_fk_tune_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m022_2026_04_insert_ids_from_user_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m023_2026_04_add_fk_user_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m024_2026_04_insert_ids_from_version_into_globally_unique_id', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m025_2026_04_add_fk_version_id_key', '2026-04-23 23:47:34+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m026_2026_04_split_user_yaml_into_fields', '2026-04-28 09:45:10.421107+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m027_2026_04_split_role_json_into_fields', '2026-04-28 21:45:03.248478+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m028_2026_04_add_remember_me_tokens_table', '2026-04-28 21:51:55.458519+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m029_2026_04_drop_remember_me_tokens_column', '2026-04-28 21:51:55.460895+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m030_2026_05_split_person_json_into_fields', '2026-05-05 15:34:03.454472+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m031_2026_05_split_source_json_into_fields', '2026-05-07 20:33:37.340054+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m032_2026_05_split_dance_json_into_fields', '2026-05-09 13:16:31.282711+00');
INSERT INTO "dancelor"."migrations" ("name", "applied_at") VALUES ('m033_2026_05_split_tune_json_into_fields', '2026-05-10 11:48:14.488321+00');


--
-- Data for Name: person; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."person" ("id", "name", "scddb_id", "composed_tunes_are_public", "published_tunes_are_public", "created_at", "modified_at") VALUES ('4plf-srss-ihav', 'Davey Arthur', NULL, true, false, '2018-12-07 00:18:53', '2023-06-25 14:51:15');
INSERT INTO "dancelor"."person" ("id", "name", "scddb_id", "composed_tunes_are_public", "published_tunes_are_public", "created_at", "modified_at") VALUES ('8h62-3eis-xfem', 'Mervyn C Short', 347, false, false, '2023-07-03 14:17:45', '2023-07-03 14:17:45');
INSERT INTO "dancelor"."person" ("id", "name", "scddb_id", "composed_tunes_are_public", "published_tunes_are_public", "created_at", "modified_at") VALUES ('uwoe-u6ij-ikgp', 'Nicolas “Niols” Jeannerod', 11781, true, false, '2018-10-12 09:50:54', '2023-06-25 14:51:15');
INSERT INTO "dancelor"."person" ("id", "name", "scddb_id", "composed_tunes_are_public", "published_tunes_are_public", "created_at", "modified_at") VALUES ('9fdg-glrm-0zoi', 'John Doe', 129872873, false, false, '2026-05-09 13:11:10.200905', '2026-05-09 13:11:10.200905');


--
-- Data for Name: recommended_tunes; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."recommended_tunes" ("dance_id", "tune_id") VALUES ('l02q-i1j0-qpoi', 'qdod-ad7l-8gr2');
INSERT INTO "dancelor"."recommended_tunes" ("dance_id", "tune_id") VALUES ('0xf7-xwz9-1fhj', 'rifw-ul36-3uq5');


--
-- Data for Name: remember_me_tokens; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."remember_me_tokens" ("user_id", "key", "hash", "max_date") VALUES ('lt3h-edgt-ac97', '33733a85f3dd0c049d7c00bb498c1cfc', '$argon2id$v=19$m=65536,t=2,p=1$LZJ1jidMtFWhex5/c37KNw$kTcM92VAz9mjp9RKIt1xVRf/tdTmQs8vhhqnUlKCYOg', '2088-11-06 11:36:51');


--
-- Data for Name: set; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."set" ("id", "json") VALUES ('ului-yd9x-o35w', '{"value":{"name":"Tam Lin Thrice","conceptors":["uwoe-u6ij-ikgp"],"kind":"3x32R","versions-and-parameters":[["xzzb-wasm-babe",{}],["xzzb-wasm-babe",{"transposition":2}],["xzzb-wasm-babe",{"transposition":7}]],"order":"1,2,3"},"meta":{"created-at":"2023-05-02T11:16:55+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":{"owners":["lt3h-edgt-ac97"],"visibility":["Everyone"]}}');
INSERT INTO "dancelor"."set" ("id", "json") VALUES ('wrwk-cz9g-g3wi', '{"value":{"name":"A Private Set","conceptors":["uwoe-u6ij-ikgp"],"kind":"3x32R","versions-and-parameters":[],"order":"1"},"meta":{"created-at":"2023-05-02T11:16:55+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":{"owners":["lt3h-edgt-ac97"],"visibility":["Owners_only"]}}');


--
-- Data for Name: source; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."source" ("id", "cover", "name", "short_name", "scddb_id", "description", "date", "created_at", "modified_at") VALUES ('2f8s-90v8-33do', NULL, 'The Tam Lin Source', 'Tam', NULL, 'this is a description', '2012-03', '2026-05-05 20:27:49', '2026-05-05 20:27:49');
INSERT INTO "dancelor"."source" ("id", "cover", "name", "short_name", "scddb_id", "description", "date", "created_at", "modified_at") VALUES ('2wrv-25yu-yc07', NULL, 'Sourceyyyyyyyy', 'Sourcey', NULL, 'sdk', NULL, '2026-05-10 11:47:17.753285', '2026-05-10 11:47:17.753285');


--
-- Data for Name: source_editors; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."source_editors" ("source_id", "person_id") VALUES ('2f8s-90v8-33do', '4plf-srss-ihav');
INSERT INTO "dancelor"."source_editors" ("source_id", "person_id") VALUES ('2f8s-90v8-33do', '8h62-3eis-xfem');
INSERT INTO "dancelor"."source_editors" ("source_id", "person_id") VALUES ('2wrv-25yu-yc07', '9fdg-glrm-0zoi');


--
-- Data for Name: tune; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."tune" ("id", "name", "kind", "remark", "scddb_id", "date", "created_at", "modified_at") VALUES ('qdod-ad7l-8gr2', 'Tam Lin', 'R', '', NULL, NULL, '2018-12-07 01:18:53', '2023-06-25 15:51:15');
INSERT INTO "dancelor"."tune" ("id", "name", "kind", "remark", "scddb_id", "date", "created_at", "modified_at") VALUES ('rifw-ul36-3uq5', 'A439', 'strathspey', '', 2398472, NULL, '2026-05-10 12:36:29', '2026-05-10 12:36:29');


--
-- Data for Name: tune_composers; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."tune_composers" ("tune_id", "index", "composer_id", "details") VALUES ('qdod-ad7l-8gr2', 0, '4plf-srss-ihav', '');
INSERT INTO "dancelor"."tune_composers" ("tune_id", "index", "composer_id", "details") VALUES ('rifw-ul36-3uq5', 0, 'uwoe-u6ij-ikgp', '');


--
-- Data for Name: tune_extra_names; Type: TABLE DATA; Schema: dancelor; Owner: -
--



--
-- Data for Name: user; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."user" ("id", "username", "password", "password_reset_token_hash", "password_reset_token_max_date", "created_at", "modified_at", "role", "omniscience", "person_id") VALUES ('lt3h-edgt-ac97', 'Niols', '$argon2id$v=19$m=65536,t=2,p=1$mm4GoaR1lz2r6jJf2OomVA$VwSQPpYI6Clwh8xdoOBcwX2BFH8VCv3B++Tx1G5B11w', NULL, NULL, '2025-04-13 16:48:00', '2025-04-13 16:48:00', 2, false, 'uwoe-u6ij-ikgp');


--
-- Data for Name: version; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."version" ("id", "json") VALUES ('xzzb-wasm-babe', '{"value":{"tune":"qdod-ad7l-8gr2","key":"Dm","disambiguation":"Niols''s Version","arrangers":["uwoe-u6ij-ikgp"],"sources":[{"source":"2f8s-90v8-33do","structure":"AABB"}],"content":["Monolithic",{"bars":32,"structure":"AABB","lilypond":"\\relative c'' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    \\repeat volta 2 {\n      \\partial 8 r8 |\n      a4 d8 a f'' a, d a |\n      bes4 d8 bes f'' bes, d bes |\n      c4 e8 c g'' c, e g |\n      f8 e d c d c a g |\n      \\break\n\n      a4 d8 a f'' a, d a |\n      bes4 d8 bes f'' bes, d bes |\n      c4 e8 c g'' c, e g |\n      f8 e d c d4.\n    } \\break\n\n    \\repeat volta 2 {\n      a''8 |\n      d8 a a a f a d, a'' |\n      d8 a a a f a d, a'' |\n      c8 g g g c g e'' g, |\n      c8 g g g c[ r c cis] |\n      \\break\n\n      d8 a a a f a d, a'' |\n      d8 a a a f[ a] d, r |\n      bes8 a bes c d c d e |\n      f8 e d c a[ d] d\n    }\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      s8 |\n      d1:m | bes | c | d2:m a:m |\n      d1:m | bes | c | a2:m d4.:m\n\n      s8 |\n      d1:m | s | c | s |\n      d1:m | s | g:m | a2:m d4.:m\n    }\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      s8 |\n      s1 | \\parenthesize g:m | s | s |\n      s1 | \\parenthesize g:m | s | s2 s4.\n\n      s8 |\n      s1 | s | s | s |\n      \\parenthesize bes1 | s | s | s2 s4.\n    }\n  }\n>>\n"}]},"meta":{"created-at":"2023-04-04T18:45:27+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":["Public"]}');
INSERT INTO "dancelor"."version" ("id", "json") VALUES ('xsbz-vqy7-xj3s', '{"value":{"tune":"rifw-ul36-3uq5","key":"Dm","sources":[{"source":"2wrv-25yu-yc07","structure":"AAAA"}],"content":["No_content"]},"meta":{"created-at":"2026-05-10T11:47:35","modified-at":"2026-05-10T11:47:35"},"access":["Public"]}');


--
-- Name: book idx_16409_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."book"
    ADD CONSTRAINT "idx_16409_primary" PRIMARY KEY ("id");


--
-- Name: dance idx_16414_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."dance"
    ADD CONSTRAINT "idx_16414_primary" PRIMARY KEY ("id");


--
-- Name: globally_unique_id idx_16419_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."globally_unique_id"
    ADD CONSTRAINT "idx_16419_primary" PRIMARY KEY ("id");


--
-- Name: migrations idx_16422_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."migrations"
    ADD CONSTRAINT "idx_16422_primary" PRIMARY KEY ("name");


--
-- Name: person idx_16426_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."person"
    ADD CONSTRAINT "idx_16426_primary" PRIMARY KEY ("id");


--
-- Name: set idx_16431_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."set"
    ADD CONSTRAINT "idx_16431_primary" PRIMARY KEY ("id");


--
-- Name: source idx_16436_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."source"
    ADD CONSTRAINT "idx_16436_primary" PRIMARY KEY ("id");


--
-- Name: tune idx_16441_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune"
    ADD CONSTRAINT "idx_16441_primary" PRIMARY KEY ("id");


--
-- Name: user idx_16446_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."user"
    ADD CONSTRAINT "idx_16446_primary" PRIMARY KEY ("id");


--
-- Name: version idx_16451_primary; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."version"
    ADD CONSTRAINT "idx_16451_primary" PRIMARY KEY ("id");


--
-- Name: user user_username_key; Type: CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."user"
    ADD CONSTRAINT "user_username_key" UNIQUE ("username");


--
-- Name: book fk_book_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."book"
    ADD CONSTRAINT "fk_book_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: dance_devisers fk_dance_devisers_dance_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."dance_devisers"
    ADD CONSTRAINT "fk_dance_devisers_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dancelor"."dance"("id");


--
-- Name: dance_devisers fk_dance_devisers_deviser_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."dance_devisers"
    ADD CONSTRAINT "fk_dance_devisers_deviser_id" FOREIGN KEY ("deviser_id") REFERENCES "dancelor"."person"("id");


--
-- Name: dance_extra_names fk_dance_extra_names_dance_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."dance_extra_names"
    ADD CONSTRAINT "fk_dance_extra_names_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dancelor"."dance"("id");


--
-- Name: dance fk_dance_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."dance"
    ADD CONSTRAINT "fk_dance_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: person fk_person_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."person"
    ADD CONSTRAINT "fk_person_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: recommended_tunes fk_recommended_tunes_dance_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."recommended_tunes"
    ADD CONSTRAINT "fk_recommended_tunes_dance_id" FOREIGN KEY ("dance_id") REFERENCES "dancelor"."dance"("id");


--
-- Name: recommended_tunes fk_recommended_tunes_tune_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."recommended_tunes"
    ADD CONSTRAINT "fk_recommended_tunes_tune_id" FOREIGN KEY ("tune_id") REFERENCES "dancelor"."tune"("id");


--
-- Name: set fk_set_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."set"
    ADD CONSTRAINT "fk_set_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: source_editors fk_source_editors_person_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."source_editors"
    ADD CONSTRAINT "fk_source_editors_person_id" FOREIGN KEY ("person_id") REFERENCES "dancelor"."person"("id");


--
-- Name: source_editors fk_source_editors_source_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."source_editors"
    ADD CONSTRAINT "fk_source_editors_source_id" FOREIGN KEY ("source_id") REFERENCES "dancelor"."source"("id");


--
-- Name: source fk_source_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."source"
    ADD CONSTRAINT "fk_source_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: tune_composers fk_tune_composers_composer_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune_composers"
    ADD CONSTRAINT "fk_tune_composers_composer_id" FOREIGN KEY ("composer_id") REFERENCES "dancelor"."person"("id");


--
-- Name: tune_composers fk_tune_composers_tune_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune_composers"
    ADD CONSTRAINT "fk_tune_composers_tune_id" FOREIGN KEY ("tune_id") REFERENCES "dancelor"."tune"("id");


--
-- Name: tune_extra_names fk_tune_extra_names_tune_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune_extra_names"
    ADD CONSTRAINT "fk_tune_extra_names_tune_id" FOREIGN KEY ("tune_id") REFERENCES "dancelor"."tune"("id");


--
-- Name: tune fk_tune_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune"
    ADD CONSTRAINT "fk_tune_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: remember_me_tokens fk_user_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."remember_me_tokens"
    ADD CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "dancelor"."user"("id");


--
-- Name: user fk_user_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."user"
    ADD CONSTRAINT "fk_user_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: user fk_user_person_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."user"
    ADD CONSTRAINT "fk_user_person_id" FOREIGN KEY ("person_id") REFERENCES "dancelor"."person"("id");


--
-- Name: version fk_version_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."version"
    ADD CONSTRAINT "fk_version_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- PostgreSQL database dump complete
--

\unrestrict ybiQOqa6jiLe8LvxFznU4q8n34iwf9VXgUBS2e7j5NMdWOEHU3ms2YSbcIZ871W

