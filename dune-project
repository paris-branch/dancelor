(lang dune 3.4)

(name dancelor)
(version 0.1)
(source (github paris-branch/dancelor))

(authors
  "Nicolas “Niols” Jeannerod <niols@niols.fr>"
  "Victor Lanvin <victor.lanvin2@gmail.com>"
  "Aymeric Fromherz <aymeric.fr@hotmail.com>")
(maintainers
  "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(license GPL-3.0-or-later)

(package
 (name dancelor)
 (synopsis "A chancelor for Scottish country dance musicians")

 (depends
  ;; Also change the OCaml version in CI.
  (ocaml               (and :build (>= 5.0)))

  (cohttp-lwt-jsoo     (>= 4.0.0))
  (cohttp-lwt-unix     (>= 4.0.0))
   dates_calc
   ISO8601
  (js_of_ocaml-ppx      :build)
   js_of_ocaml-tyxml
   logs
  (lwt_ppx              :build)
   lwt_react
   ppx_blob
   ppx_deriving_yojson
   ppx_import
  (ppx_inline_test     (or :build :with-test))
   ppx_import
   ppx_fields_conv
  (ppx_monad           (and :build (>= 0.2.0)))
   slug
   yaml

  ;; documentation and tests
  (alcotest             :with-test)
  (odoc                 :with-doc)
   ppx_deriving_qcheck
   qcheck
  (qcheck-alcotest      :with-test)

   ;; used for vendored `ppx_variants_conv`
   variantslib))

(using menhir 2.1)
(generate_opam_files)
(formatting (enabled_for dune))
