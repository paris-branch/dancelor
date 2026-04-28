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
    "json" json NOT NULL
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
    "json" json NOT NULL
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
    "json" json NOT NULL,
    "cover" "bytea"
);


--
-- Name: tune; Type: TABLE; Schema: dancelor; Owner: -
--

CREATE TABLE "dancelor"."tune" (
    "id" character varying(14) NOT NULL,
    "json" json NOT NULL
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
    "role" json NOT NULL,
    "remember_me_tokens" json NOT NULL,
    "created_at" timestamp without time zone NOT NULL,
    "modified_at" timestamp without time zone NOT NULL
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

INSERT INTO "dancelor"."dance" ("id", "json") VALUES ('l02q-i1j0-qpoi', '{"value":{"kind":"8 x 32 R","devisers":["8h62-3eis-xfem"],"names":["The Architect"]},"meta":{"created-at":"2023-12-21T17:11:33","modified-at":"2023-12-21T17:11:33"},"access":["Public"]}');


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


--
-- Data for Name: person; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."person" ("id", "json") VALUES ('4plf-srss-ihav', '{"value":{"name":"Davey Arthur","composed_tunes_are_public":true},"meta":{"created-at":"2018-12-07T01:18:53+01:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":["Public"]}');
INSERT INTO "dancelor"."person" ("id", "json") VALUES ('8h62-3eis-xfem', '{"value":{"name":"Mervyn C Short","scddb-id":347},"meta":{"created-at":"2023-07-03T14:17:45","modified-at":"2023-07-03T14:17:45"},"access":["Public"]}');
INSERT INTO "dancelor"."person" ("id", "json") VALUES ('uwoe-u6ij-ikgp', '{"value":{"name":"Nicolas “Niols” Jeannerod","scddb-id":11781,"composed_tunes_are_public":true,"user":"lt3h-edgt-ac97"},"meta":{"created-at":"2018-10-12T11:50:54+02:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":["Public"]}');


--
-- Data for Name: set; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."set" ("id", "json") VALUES ('ului-yd9x-o35w', '{"value":{"name":"Tam Lin Thrice","conceptors":["uwoe-u6ij-ikgp"],"kind":"3x32R","versions-and-parameters":[["xzzb-wasm-babe",{}],["xzzb-wasm-babe",{"transposition":2}],["xzzb-wasm-babe",{"transposition":7}]],"order":"1,2,3"},"meta":{"created-at":"2023-05-02T11:16:55+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":{"owners":["lt3h-edgt-ac97"],"visibility":["Everyone"]}}');
INSERT INTO "dancelor"."set" ("id", "json") VALUES ('wrwk-cz9g-g3wi', '{"value":{"name":"A Private Set","conceptors":["uwoe-u6ij-ikgp"],"kind":"3x32R","versions-and-parameters":[],"order":"1"},"meta":{"created-at":"2023-05-02T11:16:55+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":{"owners":["lt3h-edgt-ac97"],"visibility":["Owners_only"]}}');


--
-- Data for Name: source; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."source" ("id", "json", "cover") VALUES ('2f8s-90v8-33do', '{"value":{"name":"The Tam Lin Source"},"meta":{"created-at":"2025-04-12T18:45:27+00:00","modified-at":"2025-04-12T18:45:27+00:00"},"access":["Public"]}', NULL);


--
-- Data for Name: tune; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."tune" ("id", "json") VALUES ('qdod-ad7l-8gr2', '{"value":{"names":["Tam Lin"],"kind":"R","composers":[{"composer":"4plf-srss-ihav","details":""}],"dances":["l02q-i1j0-qpoi"]},"meta":{"created-at":"2018-12-07T01:18:53+01:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":["Public"]}');


--
-- Data for Name: user; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."user" ("id", "username", "password", "password_reset_token_hash", "password_reset_token_max_date", "role", "remember_me_tokens", "created_at", "modified_at") VALUES ('lt3h-edgt-ac97', 'Niols', '$argon2id$v=19$m=65536,t=2,p=1$mm4GoaR1lz2r6jJf2OomVA$VwSQPpYI6Clwh8xdoOBcwX2BFH8VCv3B++Tx1G5B11w', NULL, NULL, '["Normal_user"]', '{}', '2025-04-13 16:48:00', '2025-04-13 16:48:00');


--
-- Data for Name: version; Type: TABLE DATA; Schema: dancelor; Owner: -
--

INSERT INTO "dancelor"."version" ("id", "json") VALUES ('xzzb-wasm-babe', '{"value":{"tune":"qdod-ad7l-8gr2","key":"Dm","disambiguation":"Niols''s Version","arrangers":["uwoe-u6ij-ikgp"],"sources":[{"source":"2f8s-90v8-33do","structure":"AABB"}],"content":["Monolithic",{"bars":32,"structure":"AABB","lilypond":"\\relative c'' <<\n  {\n    \\clef treble\n    \\key d \\minor\n    \\time 4/4\n\n    \\repeat volta 2 {\n      \\partial 8 r8 |\n      a4 d8 a f'' a, d a |\n      bes4 d8 bes f'' bes, d bes |\n      c4 e8 c g'' c, e g |\n      f8 e d c d c a g |\n      \\break\n\n      a4 d8 a f'' a, d a |\n      bes4 d8 bes f'' bes, d bes |\n      c4 e8 c g'' c, e g |\n      f8 e d c d4.\n    } \\break\n\n    \\repeat volta 2 {\n      a''8 |\n      d8 a a a f a d, a'' |\n      d8 a a a f a d, a'' |\n      c8 g g g c g e'' g, |\n      c8 g g g c[ r c cis] |\n      \\break\n\n      d8 a a a f a d, a'' |\n      d8 a a a f[ a] d, r |\n      bes8 a bes c d c d e |\n      f8 e d c a[ d] d\n    }\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      s8 |\n      d1:m | bes | c | d2:m a:m |\n      d1:m | bes | c | a2:m d4.:m\n\n      s8 |\n      d1:m | s | c | s |\n      d1:m | s | g:m | a2:m d4.:m\n    }\n  }\n\n  \\new ChordNames {\n    \\chordmode {\n      s8 |\n      s1 | \\parenthesize g:m | s | s |\n      s1 | \\parenthesize g:m | s | s2 s4.\n\n      s8 |\n      s1 | s | s | s |\n      \\parenthesize bes1 | s | s | s2 s4.\n    }\n  }\n>>\n"}]},"meta":{"created-at":"2023-04-04T18:45:27+00:00","modified-at":"2023-06-25T16:51:15+02:00"},"access":["Public"]}');


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
-- Name: set fk_set_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."set"
    ADD CONSTRAINT "fk_set_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: source fk_source_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."source"
    ADD CONSTRAINT "fk_source_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: tune fk_tune_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."tune"
    ADD CONSTRAINT "fk_tune_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: user fk_user_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."user"
    ADD CONSTRAINT "fk_user_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: version fk_version_id; Type: FK CONSTRAINT; Schema: dancelor; Owner: -
--

ALTER TABLE ONLY "dancelor"."version"
    ADD CONSTRAINT "fk_version_id" FOREIGN KEY ("id") REFERENCES "dancelor"."globally_unique_id"("id") ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- PostgreSQL database dump complete
--

\unrestrict ybiQOqa6jiLe8LvxFznU4q8n34iwf9VXgUBS2e7j5NMdWOEHU3ms2YSbcIZ871W

