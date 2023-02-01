{ ... }: {
  perSystem = { inputs', pkgs, ... }:
    ## NOTE: The use of `../.` matters because the path is taken as relative to
    ## the current file, and therefore to `/.nix`.
    let
      scope = inputs'.opam-nix.lib.buildOpamProject { inherit pkgs; } "dancelor"
        ../. {
          merlin = "*";
          ocaml-base-compiler = "*";
          ocaml-lsp-server = "*";
          ocp-indent = "*";
          utop = "*";
        };
    in { packages.dancelor = scope.dancelor // { inherit scope; }; };
}
