(lang dune 2.5)

(name dancelor)
(source (uri https://gitlab.vlanvin.fr/paris-band/dancelor))

(authors
 "Nicolas “Niols” Jeannerod <niols@niols.fr>"
 "Victor Lanvin <victor.lanvin2@gmail.com>"
)
(license GPL3)

(maintainers "Nicolas “Niols” Jeannerod <niols@niols.fr>")
(generate_opam_files)

(allow_approximate_merlin)

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
  (yojson              (>= 1.6.0))))
(using menhir 2.1)
