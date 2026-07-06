-- NOTE: For schema elements that sqlgg cannot parse. This file will
-- be included after `schema.sql` in tests only.

CREATE INDEX "idx_person_name" ON "person" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_person_name_search" ON "person" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_dance_name" ON "dance" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_dance_name_search" ON "dance" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_source_name" ON "source" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_source_name_search" ON "source" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_tune_name" ON "tune" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_tune_name_search" ON "tune" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_set_name" ON "set" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_set_name_search" ON "set" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_book_name" ON "book" USING GIN ("name" "public"."gin_trgm_ops");
CREATE INDEX "idx_book_name_search" ON "book" USING GIN ("name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_user_username" ON "user" USING GIN ("username" "public"."gin_trgm_ops");
CREATE INDEX "idx_user_username_search" ON "user" USING GIN ("username_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_dance_extra_names_extra_name" ON "dance_extra_names" USING GIN ("extra_name" "public"."gin_trgm_ops");
CREATE INDEX "idx_dance_extra_names_extra_name_search" ON "dance_extra_names" USING GIN ("extra_name_search" "public"."gin_trgm_ops");
CREATE INDEX "idx_tune_extra_names_extra_name" ON "tune_extra_names" USING GIN ("extra_name" "public"."gin_trgm_ops");
CREATE INDEX "idx_tune_extra_names_extra_name_search" ON "tune_extra_names" USING GIN ("extra_name_search" "public"."gin_trgm_ops");
