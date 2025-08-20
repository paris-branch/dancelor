{
  perSystem =
    { self', pkgs, ... }:
    {
      packages.default = pkgs.ocamlPackages.buildDunePackage {
        pname = "dancelor";
        version = "dev";
        src = ../.;

        nativeBuildInputs =
          (with pkgs.ocamlPackages; [
            menhir
            js_of_ocaml
          ])
          ++ (with pkgs; [ sassc ]);

        buildInputs = with pkgs.ocamlPackages; [
          self'.packages.ocaml-argon2

          cohttp-lwt-jsoo
          cohttp-lwt-unix
          dates_calc
          iso8601
          js_of_ocaml-ppx
          js_of_ocaml-tyxml
          logs
          lwt_ppx
          lwt_react
          ppx_blob
          ppx_deriving_yojson
          ppx_import
          ppx_inline_test
          ppx_fields_conv
          ppx_monad
          ppx_variants_conv
          slug
          yaml

          # documentation and tests
          alcotest
          odoc
          ppx_deriving_qcheck
          qcheck
          qcheck-alcotest
        ];
      };

      packages.ocaml-argon2 = pkgs.ocamlPackages.buildDunePackage {
        pname = "argon2";
        version = "dev";
        src = pkgs.fetchFromGitHub {
          owner = "khady";
          repo = "ocaml-argon2";
          rev = "1.0.2";
          sha256 = "sha256-m5yOMT33Z9LfjQg6QRBW6mjHNyIySq6somTFuGmL9xI=";
        };

        propagatedBuildInputs =
          (with pkgs; [ libargon2 ])
          ++ (with pkgs.ocamlPackages; [
            ctypes-foreign
          ]);

        buildInputs = with pkgs.ocamlPackages; [
          ctypes
          dune-configurator
          result
        ];
      };
    };
}
