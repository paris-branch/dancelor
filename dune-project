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
  (alcotest             :with-test)
  (cohttp-lwt-jsoo     (>= 4.0.0))
  (cohttp-lwt-unix     (>= 4.0.0))
   dates_calc
  (dune                (and :build (< 3.7.0)))
   ISO8601
   js_of_ocaml
   js_of_ocaml-lwt
  (js_of_ocaml-ppx      :build)
   js_of_ocaml-tyxml
  (lwt_ppx              :build)
   lwt_react
  (ocaml               (and :build (>= 4.08.0)))
  (odoc                 :with-doc)
   ppx_deriving
   ppx_deriving_qcheck
   ppx_deriving_yojson
  (ppx_inline_test     (or :build :with-test))
   ppx_import
  (ppx_monad           (and :build (>= 0.2.0)))
   qcheck
  (qcheck-alcotest      :with-test)
   variantslib
   slug
   yaml
  (yojson              (>= 1.6.0))))

(using menhir 2.1)
(generate_opam_files)
(formatting (enabled_for dune))
