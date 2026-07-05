-- NOTE: For schema elements that sqlgg cannot parse. This file will
-- be included before `schema.sql` in tests only.

-- FIXME: Upstream to sqlgg.
CREATE EXTENSION IF NOT EXISTS pg_trgm;

CREATE EXTENSION IF NOT EXISTS unaccent WITH SCHEMA public;

CREATE FUNCTION make_name_search(TEXT) RETURNS TEXT
           LANGUAGE "sql" IMMUTABLE PARALLEL SAFE
	   AS $$ SELECT regexp_replace(lower("public"."unaccent"($1)), '^(the|an?)\s+(.+)$', '\2, \1') $$;
