(lang dune 2.5)

(homepage https://gitlab.vlanvin.fr/paris-band/dancelor)
(source (uri git+ssh://git@gitlab.vlanvin.fr:2022/paris-band/dancelor.git))
(bug_reports https://gitlab.vlanvin.fr/paris-band/dancelor/issues)

(authors
  "Nicolas “Niols” Jeannerod <niols@niols.fr>"
  "Victor Lanvin <victor.lanvin2@gmail.com>")
(maintainers
  "Nicolas “Niols” Jeannerod <niols@niols.fr>")

(license GPL3)

(package
 (name dancelor)
 (version 0.1)
 (synopsis "A chancelor for RSCDS musicians")

 (depends
  (cohttp-lwt-jsoo     (>= 4.0.0))
  (cohttp-lwt-unix     (>= 4.0.0))
  (dune                 :build)
   js_of_ocaml
   js_of_ocaml-lwt
  (js_of_ocaml-ppx      :build)
  (lwt_ppx              :build)
  (ocaml               (and :build (>= 4.08.0)))
  (odoc                 :with-doc)
  (ppx_blob             :build)
   ppx_deriving
   ppx_deriving_yojson
  (ppx_inline_test     (or :build :with-test))
  (ppx_monad            :build)
   slug
  (yojson              (>= 1.6.0))))

(using menhir 2.1)
(generate_opam_files)
(allow_approximate_merlin)
