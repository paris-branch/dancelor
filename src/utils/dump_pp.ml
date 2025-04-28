(** Small utility to read a binary OCaml file and dump its AST. It is
    particularly useful when hoping to inspect the output of a PPX called by a
    `(preprocess)` Dune stanza.

    Recommended usage:

        dune exec src/utils/dump_pp.exe -- _build/path/to/file.pp.ml
        dune exec src/utils/dump_pp.exe -- _build/path/to/file.pp.ml | topiary fmt -l ocaml | bat -l ocaml

    Taken from https://discuss.ocaml.org/t/collecting-preprocessor-output/3575/2 *)

let () =
  let ast = Pparse.read_ast Structure Sys.argv.(1) in
  Format.printf "%a\n" Pprintast.structure ast
